use rand::prelude::*;
use rust_socketio::payload::Payload;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::collections::VecDeque;
use hashbrown::HashMap;
use std::fs;
use std::path::PathBuf;
use std::sync::{Arc, Condvar, Mutex};
use std::io::{BufRead, BufReader};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[derive(clap::Parser, Debug)]
struct Args {
    #[arg(short, long)]
    input: Vec<PathBuf>,
    #[arg(short, long)]
    data_set: Option<PathBuf>,
    #[arg(long, default_value_t = 3)]
    depth: usize,
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
    #[arg(short, long, env = "RAMBLER_KEY")]
    key: String,
    #[arg(short, long)]
    room: Vec<u64>,
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(from = "Vec<String>")]
#[serde(into = "Vec<String>")]
struct TokenData {
    vec: Vec<String>,
    map: HashMap<String, usize>,
}

struct TokenSplit<I> {
    inner: I,
    string: String,
    space_prev: bool,
}

#[derive(Serialize, Deserialize)]
struct DataNode {
    // #[serde(default = "one")]
    // #[serde(skip_serializing_if = "is_one")]
    // #[serde(rename = "w")]
    weight: usize,
    // #[serde(rename = "m")]
    map: HashMap<usize, DataNode>,
}

#[derive(Serialize, Deserialize)]
struct DataSet {
    root: DataNode,
    data: TokenData,
}

struct Generator {
    max: usize,
    depth: usize,
    data_set: DataSet,
}

/*
fn one() -> usize {
    1
}

fn is_one(v: &usize) -> bool {
    *v == 1
}
*/

fn token_split<I: IntoIterator<Item = char>>(input: I) -> TokenSplit<I> {
    TokenSplit {
        inner: input,
        string: String::new(),
        space_prev: true,
    }
}

fn token_join(tokens: &[String]) -> String {
    let mut result = String::new();
    let mut space_prev = true;

    for token in tokens {
        let space_curr = !token.chars().any(char::is_alphanumeric);

        if !space_prev && !space_curr {
            result.push(' ');
        }

        space_prev = space_curr;
        result.push_str(&token);
    }

    result
}

impl TokenData {
    fn new() -> TokenData {
        TokenData {
            vec: Vec::new(),
            map: HashMap::new(),
        }
    }

    fn insert(&mut self, token: String) -> usize {
        *self.map.entry(token)
            .or_insert_with_key(|token| {
                self.vec.push(token.clone());
                self.vec.len() - 1
            })
    }

    fn get_by_token(&self, token: &str) -> Option<usize> {
        self.map.get(token).copied()
    }

    fn get_by_index(&self, index: usize) -> &str {
        &self.vec[index]
    }

    fn shrink(&mut self) {
        self.vec.shrink_to_fit();
        self.map.shrink_to_fit();
    }
}

impl From<Vec<String>> for TokenData {
    fn from(vec: Vec<String>) -> TokenData {
        let mut map = HashMap::new();

        for (i, token) in vec.iter().enumerate() {
            map.insert(token.clone(), i);
        }

        TokenData {
            vec,
            map,
        }
    }
}

impl Into<Vec<String>> for TokenData {
    fn into(self) -> Vec<String> {
        self.vec
    }
}

impl<I: Iterator<Item = char>> Iterator for TokenSplit<I> {
    type Item = String;
    
    fn next(&mut self) -> Option<String> {
        while let Some(c) = self.inner.next() {
            let space_curr = !c.is_alphanumeric();
            let mut result = None;

            if space_curr != self.space_prev {
                let string = self.string.clone();
                self.string.clear();

                if !string.chars().all(char::is_whitespace) {
                    result = Some(string);
                }
            }

            self.space_prev = space_curr;
            self.string.push(c);

            if result.is_some() {
                return result;
            }
        }
        
        let string = std::mem::take(&mut self.string);

        if !string.chars().all(char::is_whitespace) {
            Some(string)
        } else {
            None
        }
    }
}

impl DataNode {
    fn new() -> DataNode {
        DataNode {
            weight: 0,
            map: HashMap::new(),
        }
    }

    fn with_tokens(tokens: impl IntoIterator<Item = usize>) -> DataNode {
        let mut tokens = tokens.into_iter();

        let map = if let Some(first) = tokens.next() {
            let mut map = HashMap::new();
            map.insert(first, DataNode::with_tokens(tokens));
            map
        } else {
            HashMap::new()
        };

        DataNode {
            weight: 1,
            map,
        }
    }

    fn insert(&mut self, tokens: impl IntoIterator<Item = usize>) {
        use hashbrown::hash_map::Entry;

        let mut tokens = tokens.into_iter();

        self.weight += 1;

        if let Some(first) = tokens.next() {
            match self.map.entry(first) {
                Entry::Occupied(mut occ) => { occ.get_mut().insert(tokens); },
                Entry::Vacant(vac) => { vac.insert(DataNode::with_tokens(tokens)); },
            }
        }
    }

    fn get(&self, tokens: &[usize]) -> Option<&DataNode> {
        if let Some((first, rest)) = tokens.split_first() {
            self.map.get(first).and_then(|node| node.get(rest))
        } else {
            Some(self)
        }
    }

    fn shrink(&mut self) {
        for node in self.map.values_mut() {
            node.shrink();
        }
    }
}

impl DataSet {
    fn new() -> DataSet {
        DataSet {
            root: DataNode::new(),
            data: TokenData::new(),
        }
    }

    fn insert(&mut self, tokens: impl IntoIterator<Item = String>, max: usize) {
        let mut buffer = VecDeque::with_capacity(max + 1);
        let mut tokens = tokens.into_iter();

        while buffer.len() < max {
            if let Some(token) = tokens.next() {
                buffer.push_back(self.data.insert(token));
            } else {
                break;
            }
        }

        while let Some(token) = tokens.next() {
            buffer.push_back(self.data.insert(token));
            self.root.insert(buffer.iter().cloned());
            buffer.pop_front();
        }

        while buffer.len() > 0 {
            self.root.insert(buffer.iter().cloned());
            buffer.pop_front();
        }
    }

    fn get(&self, tokens: &[String]) -> Option<&DataNode> {
        let mut indices = Vec::new();

        for token in tokens {
            if let Some(index) = self.data.get_by_token(token) {
                indices.push(index);
            } else {
                indices.clear();
            }
        }

        for index in 0..indices.len() {
            if let Some(node) = self.root.get(&indices[index..]) {
                if !node.map.is_empty() {
                    return Some(node);
                }
            }
        }

        None
    }

    fn shrink(&mut self) {
        self.root.shrink();
        self.data.shrink();
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

    fn next(&self, tokens: &[String]) -> Option<String> {
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
                return Some(self.data_set.data.get_by_index(*token).to_owned());
            }

            index -= node.weight;
        }

        unreachable!();
    }

    fn gen(&self, input: &str) -> String {
        let mut tokens = token_split(input.chars()).collect::<Vec<_>>();

        while let Some(token) = self.next(&tokens) {
            tokens.push(token);
        }

        token_join(&tokens)
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

    let mut data_set = if let Some(path) = args.data_set {
        let string = fs::read(path)?;
        rmp_serde::from_slice(&string)?
    } else {
        DataSet::new()
    };

    for path in args.input {
        let file = fs::File::open(&path)?;
        let reader = BufReader::new(file);
        let chars = reader.lines().flat_map(|s| s.unwrap().chars().chain(std::iter::once('\n')).collect::<Vec<_>>());
        data_set.insert(token_split(chars), args.depth);
    }

    data_set.shrink();

    match args.action {
        Action::Write { output } => {
            let string = rmp_serde::to_vec(&data_set)?;
            fs::write(output, string)?;
        }
        Action::Generate { text } => {
            let generator = Generator::new(data_set, args.depth);
            println!("{}", generator.gen(&text));
        }
        Action::Bot(bot_args) => {
            let generator = Generator::new(data_set, args.depth);
            run(&bot_args, generator)?;
        }
    };

    Ok(())
}
