use rand::prelude::*;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use rust_socketio::payload::Payload;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::sync::{Arc, Condvar, Mutex};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[derive(clap::Parser, Debug)]
struct Args {
    #[arg(short, long)]
    input: Vec<PathBuf>,
    #[arg(short, long)]
    data_set: Option<PathBuf>,
    #[arg(long)]
    depth: Option<usize>,
    #[command(subcommand)]
    action: Action,
}

#[derive(clap::Subcommand, Debug)]
enum Action {
    Write {
        #[arg(short, long)]
        output: PathBuf,
    },
    Generate {
        #[arg(short, long)]
        text: String,
    },
    Bot(BotArgs),
}

#[derive(clap::Args, Debug)]
struct BotArgs {
    #[arg(short, long)]
    key: String,
    #[arg(short, long)]
    room: Vec<u64>,
}

#[derive(Clone, Hash, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Token(String);

#[derive(Serialize, Deserialize)]
struct DataNode {
    #[serde(default = "one")]
    #[serde(skip_serializing_if = "is_one")]
    #[serde(rename = "")]
    weight: usize,
    #[serde(flatten)]
    map: HashMap<Token, DataNode>,
}

#[derive(Serialize, Deserialize)]
struct DataSet {
    root: DataNode,
}

struct Generator {
    max: usize,
    depth: usize,
    data_set: DataSet,
}

fn one() -> usize {
    1
}

fn is_one(v: &usize) -> bool {
    *v == 1
}

impl Token {
    fn split(input: &str) -> Vec<Token> {
        let mut result = Vec::new();
        let mut string = String::new();
        let mut space_prev = true;

        for c in input.chars() {
            let space_curr = !c.is_alphanumeric();

            if space_curr != space_prev {
                if !string.chars().all(char::is_whitespace) {
                    result.push(Token(string));
                }

                string = String::new();
            }

            space_prev = space_curr;
            string.push(c);
        }

        if !string.chars().all(char::is_whitespace) {
            result.push(Token(string));
        }

        result
    }

    fn join(tokens: &[Token]) -> String {
        let mut result = String::new();
        let mut space_prev = true;

        for token in tokens {
            let space_curr = !token.0.chars().any(char::is_alphanumeric);

            if !space_prev && !space_curr {
                result.push(' ');
            }

            space_prev = space_curr;
            result.push_str(&token.0);
        }

        result
    }
}

impl DataNode {
    fn new() -> DataNode {
        DataNode {
            weight: 0,
            map: HashMap::new(),
        }
    }

    fn insert(&mut self, tokens: &[Token]) {
        self.weight += 1;

        if let Some((first, rest)) = tokens.split_first() {
            self.map
                .entry(first.to_owned())
                .or_insert_with(DataNode::new)
                .insert(rest);
        }
    }

    fn get(&self, tokens: &[Token]) -> Option<&DataNode> {
        if let Some((first, rest)) = tokens.split_first() {
            self.map.get(first).and_then(|node| node.get(rest))
        } else {
            Some(self)
        }
    }
}

impl DataSet {
    fn new() -> DataSet {
        DataSet {
            root: DataNode::new(),
        }
    }

    fn insert(&mut self, tokens: &[Token], max: usize) {
        for index in 0..tokens.len() {
            let end = tokens.len().min(index + max + 1);

            self.root.insert(&tokens[index..end]);
        }
    }

    fn get(&self, tokens: &[Token]) -> Option<&DataNode> {
        for index in 0..tokens.len() {
            if let Some(node) = self.root.get(&tokens[index..]) {
                if !node.map.is_empty() {
                    return Some(node);
                }
            }
        }

        None
    }
}

impl Generator {
    fn new(data_set: DataSet, depth: usize) -> Generator {
        Generator {
            max: 100,
            data_set,
            depth,
        }
    }

    fn next(&self, tokens: &[Token]) -> Option<Token> {
        if tokens.len() >= self.max {
            return None;
        }

        let begin = tokens.len().saturating_sub(self.depth);
        let options = self.data_set.get(&tokens[begin..])?;
        let options = options.map.iter().collect::<Vec<_>>();
        let total = options.iter().map(|(_, n)| n.weight).sum();
        let mut index = rand::thread_rng().gen_range(0..total);

        for (token, node) in options {
            if index < node.weight {
                return Some(token.clone());
            }

            index -= node.weight;
        }

        unreachable!();
    }

    fn gen(&self, input: &str) -> String {
        let mut tokens = Token::split(input);

        while let Some(token) = self.next(&tokens) {
            tokens.push(token);
        }

        Token::join(&tokens)
    }
}

fn run(args: &BotArgs, generator: Generator) -> Result<()> {
    let client = reqwest::blocking::Client::new();
    let bearer = format!("Bearer {}", args.key);
    let pair = Arc::new((Mutex::new(0), Condvar::new()));
    let generator = Arc::new(generator);

    for room in args.room.iter().copied() {
        let pair = Arc::clone(&pair);
        let generator = Arc::clone(&generator);

        client
            .post(format!("http://localhost:3000/chat/id/{}/members", room))
            .header("Authorization", &bearer)
            .send()?;

        rust_socketio::ClientBuilder::new("http://localhost:3000")
            .opening_header("Authorization", bearer.clone())
            .namespace("/room")
            .on("message", move |data, socket| {
                if let Payload::String(string) = data {
                    let v: Value = serde_json::from_str(&string).unwrap();
                    let content = v["content"].as_str().unwrap();

                    if content.starts_with("/ramble ") {
                        let result = generator.gen(&content[8..]);
                        socket.emit("message", json!(result)).unwrap();
                    }
                }
            })
            .on("open", move |_, socket| {
                socket.emit("join", json!({ "id": room })).unwrap();
            })
            .on("error", move |data, _| {
                eprintln!("Error: {:?}", data);
            })
            .on("close", move |_, _| {
                let (lock, cvar) = &*pair;
                let mut count = lock.lock().unwrap();
                *count += 1;
                cvar.notify_one();
            })
            .connect()?;
    }

    let (lock, cvar) = &*pair;
    let mut count = lock.lock().unwrap();

    while *count < args.room.len() {
        count = cvar.wait(count).unwrap();
    }

    Ok(())
}

fn main() -> Result<()> {
    let args = <Args as clap::Parser>::parse();
    let depth = args.depth.unwrap_or(3);

    let mut data_set = if let Some(path) = args.data_set {
        let string = fs::read(path)?;
        serde_json::from_slice(&string)?
    } else {
        DataSet::new()
    };

    for path in args.input {
        let string = fs::read_to_string(&path)?;
        let tokens = Token::split(&string);
        data_set.insert(&tokens, depth);
    }

    match args.action {
        Action::Write { output } => {
            let string = serde_json::to_vec(&data_set)?;
            fs::write(output, string)?;
        }
        Action::Generate { text } => {
            let generator = Generator::new(data_set, depth);
            println!("{}", generator.gen(&text));
        }
        Action::Bot(bot_args) => {
            let generator = Generator::new(data_set, depth);
            run(&bot_args, generator)?;
        }
    };

    Ok(())
}
