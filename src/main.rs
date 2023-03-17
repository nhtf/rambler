use clap::{Parser, Subcommand};
use rand::prelude::*;
use serde::{Deserialize, Serialize};
use serde_with::serde_as;
use std::collections::HashMap;
use std::error::Error;
use std::fs;
use std::path::PathBuf;

#[derive(Parser, Debug)]
struct Args {
    #[command(subcommand)]
    action: Action,
}

#[derive(Subcommand, Debug)]
enum Action {
    DataSet {
        #[arg(short, long)]
        input: Vec<PathBuf>,
        #[arg(short, long)]
        output: PathBuf,
    },
    Generate {
        #[arg(short, long)]
        data_set: PathBuf,
        #[arg(short, long)]
        text: String,
    },
}

#[derive(Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
struct Token(String);

#[serde_as]
#[derive(Serialize, Deserialize)]
struct DataSet {
    #[serde_as(as = "Vec<(_, _)>")]
    hash_map: HashMap<Vec<Token>, HashMap<Token, usize>>,
}

struct Generator {
    max: usize,
    context: usize,
    data_set: DataSet,
}

impl Token {
    fn split(input: &str) -> Vec<Token> {
        input
            .split_whitespace()
            .map(|s| Token(s.to_owned()))
            .collect()
    }

    fn join(tokens: &[Token]) -> String {
        tokens
            .iter()
            .map(|t| t.0.clone())
            .collect::<Vec<_>>()
            .join(" ")
    }
}

impl DataSet {
    fn new() -> DataSet {
        DataSet {
            hash_map: HashMap::new(),
        }
    }

    fn insert(&mut self, tokens: &[Token], max: usize) {
        for size in 1..=max {
            for window in tokens.windows(size + 1) {
                if let Some((last, key)) = window.split_last() {
                    let map = self.hash_map.entry(key.to_owned()).or_default();
                    *map.entry(last.clone()).or_default() += 1;
                }
            }
        }
    }

    fn get(&self, tokens: &[Token], context: usize) -> Option<&HashMap<Token, usize>> {
        for start in tokens.len().saturating_sub(context)..tokens.len() {
            if let Some(result) = self.hash_map.get(&tokens[start..]) {
                return Some(result);
            }
        }

        return None;
    }
}

impl Generator {
    fn new(data_set: DataSet, context: usize) -> Generator {
        Generator {
            max: 100,
            data_set,
            context,
        }
    }

    fn next(&self, tokens: &[Token]) -> Option<Token> {
        if tokens.len() >= self.max {
            return None;
        }

        let options = self.data_set.get(tokens, self.context)?;
        let options = options.iter().collect::<Vec<_>>();
        let mut rng = rand::thread_rng();
        options.choose(&mut rng).map(|t| t.0).cloned()
    }

    fn gen(&self, input: &str) -> String {
        let mut tokens = Token::split(input);

        while let Some(token) = self.next(&tokens) {
            tokens.push(token);
        }

        Token::join(&tokens)
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    match args.action {
        Action::DataSet { input, output } => {
            let mut data_set = DataSet::new();

            for path in input {
                let file = fs::read_to_string(&path)?;
                let tokens = Token::split(&file);
                data_set.insert(&tokens, 3);
            }

            let string = serde_json::to_string(&data_set)?;
            fs::write(output, &string)?;
        }
        Action::Generate { data_set, text } => {
            let string = fs::read_to_string(&data_set)?;
            let data_set = serde_json::from_str(&string)?;
            let generator = Generator::new(data_set, 3);
            println!("{}", generator.gen(&text));
        }
    }

    Ok(())
}
