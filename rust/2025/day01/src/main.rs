fn main() {
    let input = include_str!("../../../../inputs/2025/Day1/input.txt");

    part_one(input);
}

fn part_one(input: &str) {
    let lines = input.lines();

    let mut zeros: u32 = 0;

    lines
        .map(|line| line.split_at(1))
        .map(|(h, t)| (h, t.parse::<i32>().unwrap()))
        .fold(50, |dial, (dir, rotate)| {
            // println!("{:?}", (dir, rotate));
            let mut next_dial: i32 = if dir == "L" {
                dial - rotate
            } else {
                dial + rotate
            };

            next_dial = next_dial % 100;
            if next_dial < 0 {
                next_dial = next_dial + 100;
            }

            if next_dial == 0 {
                zeros = zeros + 1;
            }
            println!("{next_dial}");

            next_dial
        });

    println!("Num zeros: {:?}", zeros);
}
