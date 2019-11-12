use std::collections::HashSet;

fn string_to_state(state_string:&String) -> HashSet<i64> {
    let mut answer = HashSet::new();
    let mut index = 0;
    for letter in state_string.chars() {
        if letter == '#' {
            answer.insert(index);
        }
        index += 1;
    }
    answer
}

fn part2(state:&HashSet<i64>, rules:&Vec<&str>) -> i64 {
    fn further(a:&[i64]) -> bool {
        if a.len() < 3 {
            return true;
        }
        // dbg!(a);
        a[a.len()-1] != a[a.len()-2]
    }
    let mut diffs = Vec::new();
    let mut last_score = 0;
    let mut curr_score = 0;
    let mut curr_gen   = state.clone();
    while further(&diffs) {
        last_score = curr_score;
        curr_gen = next_generation(&curr_gen, &rules);
        curr_score = score(&curr_gen);
        diffs.push(curr_score-last_score);
    }
    (50000000000 - (diffs.len() as i64))*(diffs.last().unwrap()) + score(&curr_gen)
}

fn applies(rule:&str, state:&HashSet<i64>, position:i64) -> bool{
    let mut position_modifier = -2;
    for letter in rule.chars() {
        if letter == '#' {
            if !state.contains(&(position + position_modifier)) {
                return false;
            }
        } else {
            if state.contains(&(position + position_modifier)) {
                return false;
            }
        }
        position_modifier += 1;
    }
    true
}

fn next_generation(state:&HashSet<i64>, rules:&Vec<&str>) -> HashSet<i64> {
    let start_pos = state.iter().min().unwrap()-2;
    let end_pos   = state.iter().max().unwrap()+2;
    let mut ng = HashSet::new();
    for position in start_pos..end_pos {
        for rule in rules.iter() {
            if applies(&rule, &state, position) {
                ng.insert(position);
            }
        }
    }
    ng
}

fn score(state:&HashSet<i64>) -> i64 {
    state.iter().fold(0, |x,y| x+y)
}

fn generations(state:&HashSet<i64>, rules:&Vec<&str>, n: u64) -> HashSet<i64>{
    let mut ans = state.clone();
    for _i in 0..n {
        ans = next_generation(&ans, &rules);
    }
    ans
}

fn main() {
    let initial_state_string = ".##..#.#..##..##..##...#####.#.....#..#..##.###.#.####......#.......#..###.#.#.##.#.#.###...##.###.#";
    let rules = vec![".##.#", 
                     "##.#.", 
                     "##...", 
                     "..##.", 
                     "#####", 
                     "#...#", 
                     ".#...", 
                     "###.#", 
                     "#.###",                      
                     ".###.", 
                     "##.##", 
                     "#.#.#", 
                     "...#.", 
                     "..#..", 
                     "#..##", 
                     "#..#."];
    let initial_state = string_to_state(&String::from(initial_state_string));
    println!("part 1: {}", score(&generations(&initial_state, &rules, 20)));
    println!("part 2: {}", part2(&initial_state, &rules));
}