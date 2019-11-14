#[derive(Debug)]
struct Day14 {
    elves:(u64,u64),
    scores:std::collections::HashMap<u64, u64>,
    maxIndex:u64,
}

impl Day14 {
    fn new() -> Day14 {
        let mut map = std::collections::HashMap::new();
        map.insert(0,3);
        map.insert(1,7);
        Day14 {
            elves:(0,1),
            scores:map,
            maxIndex:1
        }
    }

    fn char(&self, n:u64) -> &u64{
        self.scores.get(&n).unwrap()
    }

    fn tick(&mut self) {
        // first: add new recipe(s)
        // self.scores.push_str(format!("{}", self.char(self.elves.0) + self.char(self.elves.1)).as_str());
        for letter in (self.char(self.elves.0) + self.char(self.elves.1)).to_string().chars() {
            self.maxIndex+=1;
            self.scores.insert(self.maxIndex, (letter.to_digit(10).unwrap() as u64));
        }
        // second: move the elves
        self.elves.0 += self.char(self.elves.0) + 1;
        self.elves.0 %= (self.maxIndex+1);
        self.elves.1 += self.char(self.elves.1) + 1;
        self.elves.1 %= (self.maxIndex+1);
    }
}

fn part1 (n:u64) -> Day14 {
    let mut day14 = Day14::new();
    while day14.maxIndex < n+10 {
        // dbg!(&day14);
        day14.tick();
    }
    println!("part 1: {}", (n..n+10).map(|i| day14.scores.get(&i).unwrap().to_string()).collect::<String>());
    day14
}

fn part2 (day14:&mut Day14, search: &str) -> u64 {
    let mut last_check = day14.maxIndex-12;
    let mut curr_check = day14.maxIndex-11;
    loop {
        // dbg!("worked");
        if curr_check - last_check > 1 {
            if ((last_check+1..last_check+(search.len()as u64)+1).map(|i| day14.scores.get(&i).unwrap().to_string()).collect::<String>()) == search {
                return last_check+1
            }
        }
        if ((curr_check..curr_check+(search.len() as u64)).map(|i| day14.scores.get(&i).unwrap().to_string()).collect::<String>()) == search {
            return curr_check;
        }
        last_check = curr_check;
        curr_check += 1;
        day14.tick();
    }
}

fn main() {
    let mut day14 = part1(306281);
    dbg!(part2(&mut day14, "306281"));
}
