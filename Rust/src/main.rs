/* ----------------------------------------------
	Name:   Ho Yin Ng & Filip Bajlon
	Course: CPS506, Winter 2017, Assignment #3
	Due:    2017.04.11 18:30
	Credit: This is entirely our own work. 
   ---------------------------------------------- */
use std::collections::HashMap;
use std::fmt; 
use std::io;
use std::io::prelude::*;

#[derive(Clone)]
struct Player {
		name: char,
		av: bool,
		double:bool,
		es: bool,
		loc: u16,
}

impl Player{
	fn new (n: char) -> Player{
		Player{
			name : n,
			av : false,
			double: false,
			es : false,
			loc: 0,
		}
	}
	fn give_av (&mut self){
		self.av = true;
	}
	fn give_double (&mut self){
		self.double = true;
	}
	fn give_es (&mut self){
		self.es = true;
	}
}

#[derive(Clone)]
struct GameData {
			board_x: u16,
			board_y: u16,
			turns: u16,
			won: bool,
			num_players : u16,
			players: HashMap<u16, Player>,
			snakes: HashMap<u16, u16>,
			ladders: HashMap<u16, u16>,
			powerup: HashMap<u16, char>,
			board: HashMap<u16,u16>,
			dice : Vec<u16>,

}
impl GameData {
	fn new() -> GameData {
		GameData {
			board_x: 0,
			board_y: 0,
			won: false, 
			turns: 0,
			num_players : 0,
			players: HashMap::new(),
			snakes: HashMap::new(),
			ladders: HashMap::new(),
			powerup: HashMap::new(),
			board: HashMap::new(),
			dice : Vec::new(),
		}
	}
	fn set_board(&mut self, x: String, y : String) {
		self.board_x = x.parse::<u16>().unwrap();
		self.board_y = y.parse::<u16>().unwrap();
	}
	/* Takes a list of Strings, Convert to list of Numbers */
	fn set_dice(&mut self, list : Vec<&str>  ){
		let mut temp = list.clone();
		temp.remove(0);
		self.dice = temp.iter().map(|x| x.parse::<u16>().unwrap()).collect::<Vec<u16>>();
	}
	/* Given a location, set destination */
	fn set_snake(&mut self,x : String, y: String){
		self.snakes.insert (x.parse::<u16>().unwrap(),y.parse::<u16>().unwrap());
	}
	fn set_ladder(&mut self,x : String, y: String){
		self.ladders.insert (x.parse::<u16>().unwrap(),y.parse::<u16>().unwrap());
	}
	fn set_powerup (&mut self, ptype : String, list : Vec<&str> ){
		let mut temp = list.clone();
		temp.remove(0); // !!!!!!!!!!!!!!!!!!!!!!!!!!!
		temp.remove(0);
		let location_list : Vec <u16> = temp.iter().map(|x| x.parse::<u16>().unwrap()).collect::<Vec<u16>>();
		let real_type : char = ptype.chars().next().unwrap();
		location_list.iter().fold(None, |_, &x| self.powerup.insert(x,real_type));
	}
	fn set_players (&mut self, n : String){
		self.num_players = n.parse::<u16>().unwrap();
	}
	fn set_turns (&mut self, n : String){
		self.turns = n.parse::<u16>().unwrap();
	}
	fn run_game (&mut self){
		self.initalize_game();
		self.process_game();
	}
	fn rrun_game(&mut self) -> GameData{
		let mut sgd = self.clone();
		sgd.run_game();
		return sgd;
	}
	fn initalize_game (&mut self){
		// INITALIZE PLAYERS
		//(0..self.num_players).fold(None,|_, &i| 
		for i in  0 .. self.num_players{
			self.players.insert(i,Player::new( (65+i) as u8 as char));
			self.put_player_at(1,i+1);
			if self.won == true{ break ;}
		}
	}
	fn process_game (&mut self){
		// WHILE LOOP OR SOMETHING
		let mut n_turns = self.turns.clone();
		let mut dice_counter = 0;
		let len = self.dice.len();
		while n_turns > 0 {
			n_turns = n_turns - 1;
			for i in  0 .. self.num_players{
				if self.won == true{ break; }
				let mut roll = self.dice[dice_counter];
				if let  Some(player) = self.players.get_mut(&(i+1) ){
					if player.double == true { roll = roll*2;  player.double = false }
				}
				self.forward_player_by(i+1, roll);
				dice_counter = (dice_counter + 1) % len;
			}
			if self.won == true{ break; }   
		}
	}
	fn forward_player_by (&mut self, player_number : u16, amount : u16){
		let player_index = player_number - 1;
		let mut lloc : u16 = 0;
		if let Some(player) = self.players.get(&player_index){
			lloc = player.loc;
		}
		self.put_player_at(lloc + 1 + amount , player_number);
	}
	/* player_number starts from (1.. n), player_index starts from (0..n-1) */
	fn put_player_at (&mut self, cell_number : u16, player_number : u16) {
		let player_index = player_number - 1;
		let mut dst = cell_number -1;
		let mut lloc : u16 = 0;
		let mut occupied : bool = false;
		let mut occupant : u16 = 1;
		// Check for out-of-bound
		if cell_number > self.board_x * self.board_y{
			return;
		}
		if cell_number == self.board_x * self.board_y{
			self.won= true;
		}
		// Give player power if
		if let Some(&apower) = self.powerup.get(&cell_number){
			match apower{
				'd' => if let Some(player) = self.players.get_mut(&player_index) { player.give_double();},
				'e' => if let Some(player) = self.players.get_mut(&player_index) { player.give_es();},
				'a' => if let Some(player) = self.players.get_mut(&player_index) { player.give_av();},
			 	_ => println!("ERROR HOOOMAN!"),
			}
		}
		// IF THERE IS A PLAYER IN DST
		if let Some(player) = self.board.get_mut(&dst){
			if cell_number > self.board_x * self.board_y{
				return;
			}
			occupant = *player + 1;
			occupied = true;
		}else{
			// REMOVE WHERE I WAS
			if let Some(current_player) = self.players.get(&player_index) {
				lloc =current_player.loc;
			}
		}

		if occupied == false{
			if self.ladders.contains_key(&(dst+1)){
				if let Some(p) = self.players.clone().get(&player_index){
					if let Some(ladder_tail) = self.ladders.clone().get(&(dst+1)){
						let new_dst = *ladder_tail - 1;
						let mut displacement = new_dst - dst;
						if p.es == true{
							if let Some(t) = self.players.get_mut(&player_index){
								t.es = false;
							}
							displacement = displacement*2;
						}
						dst = dst + displacement;
						self.put_player_at(dst+1,player_index+1);
						return;
					}
				}
			}
			if self.snakes.contains_key(&(dst+1)){
				if let Some(p) = self.players.clone().get(&player_index){
					if p.av == false{
						if let Some(snake_tail) = self.snakes.clone().get(&(dst+1)){
							dst = *snake_tail - 1;
							self.put_player_at(dst+1,player_index+1);
						}
					}else{
						if let Some(t) = self.players.get_mut(&player_index){
							t.av = false;
						}
					}
				}
			}
			self.board.remove(&lloc);
		}else{
			self.put_player_at(cell_number+1,occupant);
			self.put_player_at(cell_number,player_number);
			//return; // MIGHT BE MISTAKE
		}
		self.board.insert(dst, player_index);
		if let Some(current_player) = self.players.get_mut(&player_index) {
			current_player.loc = dst;
		}
	}
	fn print (self) -> String{
		let mut output_string = "".to_string();
		let mut cell : u16;
		let mut r : bool = false;
		for i in 0 .. self.board_y {
			output_string =  "".to_string() + generate_boarder(self.board_x).as_str() + output_string.as_str();
			let mut cell_bit = String::from("|");
			let mut cell_num_bit = String::from("|");
			let mut arange = (0..self.board_x).fold( vec!(), |acc, x| {let mut c = acc.clone(); c.push(x); c });
			if r == true {arange = (0..self.board_x).rev().fold( vec!(), |acc, x| {let mut c = acc.clone(); c.push(x); c }) } 
			r = !r;
			for j in arange.iter(){
				cell = i * self.board_x + j + 1;
				let number = (cell ).to_string();
				for _ in number.len()..3 { cell_num_bit.push(' ') }
				cell_num_bit.push_str(number.as_str());
				if let Some(player) = self.board.get(&(cell-1)){
					cell_bit.push(self.players.get(&(player)).unwrap().name);
				}else{ cell_bit.push(' ') }

				if let Some(power_up) = self.powerup.get(&(cell)){
					cell_bit.push(*power_up);
				}else{ cell_bit.push(' ') }

				if let Some(_) = self.snakes.get(&cell){
					cell_bit.push('S');
				}else if let Some (_) = self.ladders.get(&cell){
					cell_bit.push('L');
				}else{ cell_bit.push(' '); }
				cell_bit.push('|');
				cell_num_bit.push('|');
			}
			output_string = "".to_string() + cell_bit.as_str()+ "\n" + output_string.as_str();
			output_string = "".to_string() + cell_num_bit.as_str()+ "\n" + output_string.as_str();
		}
		output_string =  "".to_string() + generate_boarder(self.board_x).as_str() + output_string.as_str();
		if self.won == true{
			output_string = output_string + "player " + self.players.get(self.board.get(&(self.board_x*self.board_y-1)).unwrap()).unwrap().name.to_string().as_str() + " won\n"
		}
		//println!("{}", output_string);
		return output_string
	}
	fn readfrom(mut self, astr : String) ->  GameData {
		let command = astr;
		let line_ref = command.clone();
		let mut line = command.split(" ");
		match (line.next(), line.next(), line.next()) {
			(Some("board"),  Some(x), Some(y)) => self.set_board(x.to_string(),y.to_string()),
				(Some("snake"),  Some(x), Some(y)) => self.set_snake(x.to_string(),y.to_string()),
				(Some("ladder"), Some(x), Some(y)) => self.set_ladder(x.to_string(),y.to_string()),
				(Some("dice"), _, _) => self.set_dice(line_ref.split(" ").collect()),
				(Some("powerup"), Some(atype),_) => self.set_powerup(atype.to_string(),line_ref.split(" ").collect()),
				(Some("players"), Some(x), _) => self.set_players(x.to_string()),
				(Some("turns"), Some(x), _) => self.set_turns(x.to_string()),
				_ => println!(""), // UNEXPECTED HOOOMAN
		}
		self
	}

}
impl fmt::Display for GameData {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.clone().print())
	}
}
fn generate_boarder (n : u16) -> String{
	(0..n).fold("".to_string(), |acc, _| acc+ "+---" ) + "+\n"
}

fn main() {
	let mut test_gd : GameData = GameData::new();
	let stdin = io::stdin();
	for line in stdin.lock().lines() {
		test_gd = test_gd.clone().readfrom (line.unwrap());
	}
	test_gd.run_game();
	let j : String = format!("{}", test_gd);
	println!("{}",test_gd);
	
}
#[cfg(test)]
mod tests {
	use super::*;
	use super::GameData;
	#[test]
	fn it_works() {
		assert_eq!("+---+---+\n|  4|  3|\n|   |   |\n+---+---+\n|  1|  2|\n|   |   |\n+---+---+\n", format!("{}", "board 2 2".lines().fold(GameData::new(), |test_gd, command| test_gd.readfrom(command.to_string()))));
		assert_eq!("+---+---+\n|  5|  6|\n|   |   |\n+---+---+\n|  4|  3|\n|   |   |\n+---+---+\n|  1|  2|\n|   |   |\n+---+---+\n" ,format!("{}", "board 2 3".lines().fold(GameData::new(),| test_gd, command| test_gd.readfrom(command.to_string()))));
		assert_eq!( "+---+---+---+\n| 12| 11| 10|\n|B  |   |   |\n+---+---+---+\n|  7|  8|  9|\n| a |  S| e |\n+---+---+---+\n|  6|  5|  4|\n| e |  L|Ad |\n+---+---+---+\n|  1|  2|  3|\n|   |   |   |\n+---+---+---+\nplayer B won\n",format!("{}","board 3 4\nplayers 2\ndice 1 2 2 2 2\nladder 5 11\nsnake 8 4\npowerup escalator 6 9\npowerup antivenom 7\npowerup double 4\nturns 10".lines().fold(GameData::new(), |test_gd, command|  test_gd.readfrom(command.to_string()) ).rrun_game()));
	}
}


/* 
   cd existing_folder
   git init
   git remote add origin git@gitlab.scs.ryerson.ca:dmason/cps506-w2017-a4-fbajlon-hyng.git
   git add .
   git commit
   git push -u origin master
 */







