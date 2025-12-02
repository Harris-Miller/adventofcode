fn main() {
    let input = include_str!("../../../../inputs/2025/Day1/input.txt");

    part_one(input);
}

fn part_one(input: &str) {
    let lines = input.lines();

    let mut zeros: i32 = 0;

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

            // special case, I don't like this, come up with something better
            if next_dial == 100 || next_dial == -100 {
                zeros += 1;
                next_dial = 0;
                return next_dial;
            }

            zeros += (next_dial / 100).abs();
            next_dial = next_dial % 100;
            if next_dial == 0 || (next_dial < 0 && dial > 0) || (next_dial > 0 && dial < 0) {
                zeros += 1
            }

            if next_dial < 0 {
                next_dial += 100;
            }

            println!("Dial: {:?}, Num zeros: {:?}", next_dial, zeros);

            return next_dial;
        });

    println!("Num zeros: {:?}", zeros);
}
