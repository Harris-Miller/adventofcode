use std::fs;

fn parse(c: char) -> i32 {
    match c {
        '(' => 1,
        ')' => -1,
        _ => 0,
    }
}

pub fn main() {
    let file_path = "2015/inputs/Day1/input.txt";
    let contents = fs::read_to_string(file_path).unwrap();
    let values = contents.chars().map(parse);

    // part 1
    let r: i32 = values.clone().sum();
    println!("{r}");

    // part 2
    let r2 = values
        .clone()
        .scan(0, |state, x| {
            *state = *state + x;
            Some(*state)
        })
        .position(|x| x == -1)
        .map(|i| i + 1) // scan's iter does not include initial_state, so it's a single index behind
        .unwrap();
    println!("{r2}");
}
