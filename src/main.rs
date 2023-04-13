use hashbrown::{hash_map::Entry, HashMap};
use rand::prelude::*;
use rust_socketio::payload::Payload;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::collections::VecDeque;
use std::fs;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;
use std::sync::Arc;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[derive(clap::Parser, Debug)]
struct Args {
    #[arg(short, long)]
    input: Vec<PathBuf>,
    #[arg(short, long, env = "RAMBLER_DATA")]
    data_set: Option<PathBuf>,
    #[arg(long, default_value = "3")]
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
    #[arg(short, long, env = "RAMBLER_URL", default_value = "http://localhost:3000")]
    url: String,
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(from = "Vec<String>")]
#[serde(into = "Vec<String>")]
struct TokenData {
    vec: Vec<String>,
    map: HashMap<String, u32>,
}

struct TokenSplit<I> {
    inner: I,
    string: String,
    space_prev: bool,
}

#[derive(Serialize, Deserialize)]
struct DataNode {
    weight: u32,
    map: HashMap<u32, Box<DataNode>>,
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

#[derive(Debug)]
enum UpdateAction {
    Insert = 0,
    Update = 1,
    Remove = 2,
}

#[derive(Debug)]
enum UpdateSubject {
    User = 0,
    Invite = 1,
    Room = 2,
    Member = 3,
    Friend = 4,
    Message = 5,
    GameState = 6,
    Team = 7,
    Block = 8,
    Player = 9,
    Achievement = 10,
    Score = 11,
}

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

    fn insert(&mut self, token: String) -> u32 {
        *self.map.entry(token).or_insert_with_key(|token| {
            self.vec.push(token.clone());
            self.vec.len() as u32 - 1
        })
    }

    fn get_by_token(&self, token: &str) -> Option<u32> {
        self.map.get(token).copied()
    }

    fn get_by_index(&self, index: u32) -> &str {
        &self.vec[index as usize]
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
            map.insert(token.clone(), i as u32);
        }

        TokenData { vec, map }
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

    fn with_tokens(tokens: impl IntoIterator<Item = u32>) -> DataNode {
        let mut tokens = tokens.into_iter();

        let map = if let Some(first) = tokens.next() {
            let mut map = HashMap::new();
            map.insert(first, Box::new(DataNode::with_tokens(tokens)));
            map
        } else {
            HashMap::new()
        };

        DataNode { weight: 1, map }
    }

    fn insert(&mut self, tokens: impl IntoIterator<Item = u32>) {
        let mut tokens = tokens.into_iter();

        self.weight += 1;

        if let Some(first) = tokens.next() {
            match self.map.entry(first) {
                Entry::Occupied(mut occ) => {
                    occ.get_mut().insert(tokens);
                }
                Entry::Vacant(vac) => {
                    vac.insert(Box::new(DataNode::with_tokens(tokens)));
                }
            }
        }
    }

    fn get(&self, tokens: &[u32]) -> Option<&DataNode> {
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

impl UpdateAction {
    fn parse(value: u64) -> Option<UpdateAction> {
        match value {
            0 => Some(UpdateAction::Insert),
            1 => Some(UpdateAction::Update),
            2 => Some(UpdateAction::Remove),
            _ => None,
        }
    }
}

impl UpdateSubject {
    fn parse(value: u64) -> Option<UpdateSubject> {
        match value {
            0 => Some(UpdateSubject::User),
            1 => Some(UpdateSubject::Invite),
            2 => Some(UpdateSubject::Room),
            3 => Some(UpdateSubject::Member),
            4 => Some(UpdateSubject::Friend),
            5 => Some(UpdateSubject::Message),
            6 => Some(UpdateSubject::GameState),
            7 => Some(UpdateSubject::Team),
            8 => Some(UpdateSubject::Block),
            9 => Some(UpdateSubject::Player),
            10 => Some(UpdateSubject::Achievement),
            11 => Some(UpdateSubject::Score),
            _ => None,
        }
    }
}

fn run(args: &BotArgs, generator: Generator) -> Result<()> {
    let client = Arc::new(reqwest::blocking::Client::new());
    let bearer = Arc::new(format!("Bearer {}", args.key));
    let url = Arc::new(args.url.clone());
    let generator = Arc::new(generator);

    let response = client
        .get(format!("{}/user/me", url))
        .header("Authorization", &*bearer)
        .send()?;

    let me: Value = serde_json::from_str(&response.text()?)?;
    let me_id = me["id"].as_u64().unwrap();
    let me_username = me["username"].as_str().unwrap();

    println!("My name is {}", me_username);

    let response = client
        .get(format!("{}/user/me/invites", url))
        .header("Authorization", &*bearer)
        .send()?;

    let invites: Value = serde_json::from_str(&response.text()?)?;

    let invites_rooms = invites
        .as_array()
        .unwrap()
        .iter()
        .filter(|v| v["from"]["id"] != me_id && v["type"] == "ChatRoomInvite")
        .map(|v| v["room"]["id"].as_u64().unwrap())
        .collect::<Vec<_>>();

    for room in args.room.iter().copied().chain(invites_rooms) {
        println!("Joining {}", room);

        client
            .post(format!("{}/chat/{}/members", url, room))
            .header("Authorization", &*bearer)
            .send()?;
    }

    rust_socketio::ClientBuilder::new(&*url)
        .opening_header("Authorization", (*bearer).clone())
        .namespace("/update")
        .reconnect(true)
        .on("update", move |data, _| {
            if let Payload::String(string) = data {
                let v: Value = serde_json::from_str(&string).unwrap();

                let action = UpdateAction::parse(v["action"].as_u64().unwrap()).unwrap();
                let subject = UpdateSubject::parse(v["subject"].as_u64().unwrap()).unwrap();
                let value = &v["value"];

                match (action, subject) {
                    (UpdateAction::Insert, UpdateSubject::Message) => {
                        let content = value["content"].as_str().unwrap();
                        let room = value["roomId"].as_u64().unwrap();

                        if content.starts_with("/ramble ") {
                            let result = generator.gen(&content[8..]);

                            println!("Rambling about {:?}", &content[8..]);

                            client
                                .post(format!("{}/chat/{}/messages", url, room))
                                .header("Authorization", &*bearer)
                                .header("Content-Type", "application/json")
                                .body(json!({ "content": result }).to_string())
                                .send()
                                .unwrap();
                        }
                    }
                    (UpdateAction::Insert, UpdateSubject::Invite) => {
                        if value["type"] == "ChatRoomInvite" {
                            let room = value["room"]["id"].as_u64().unwrap();

                            println!("Joining {}", room);

                            client
                                .post(format!("{}/chat/{}/members", url, room))
                                .header("Authorization", &*bearer)
                                .send()
                                .unwrap();
                        }
                    }
                    (action, subject) => {
                        println!("I don't know how to {:?} a {:?}", action, subject);
                    }
                }
            }
        })
        .on("open", move |data, _| {
            eprintln!("Connection opened: {:?}", data);
        })
        .on("error", move |data, _| {
            eprintln!("Connection error: {:?}", data);
        })
        .on("close", move |data, _| {
            eprintln!("Connection closed: {:?}", data);
        })
        .connect()?;

    loop {
        std::thread::park();
    }
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
        let chars = reader.lines().flat_map(|s| {
            s.unwrap()
                .chars()
                .chain(std::iter::once('\n'))
                .collect::<Vec<_>>()
        });
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
