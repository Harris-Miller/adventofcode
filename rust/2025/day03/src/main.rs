fn main() {
    let input = include_str!("../../../../inputs/2025/Day3/sample.txt")
        .lines()
        .collect::<Vec<&str>>();

    part_one(input);
}

fn process_one(line: &str) -> u32 {
    let indexed = line.chars().enumerate();

    let result = indexed
        .reduce(|largest, current| {
            if current.1 > largest.1 {
                current
            } else {
                largest
            }
        })
        .unwrap();

    let is_last = result.0 == line.len() - 1;

    if is_last {
        let other = indexed
            .take(line.len() - 1)
            .reduce(|largest, current| {
                if current.1 > largest.1 {
                    current
                } else {
                    largest
                }
            })
            .unwrap();
        let as_string = String::from_iter(vec![other.1, result.1]);
        as_string.parse::<u32>().unwrap()
    } else {
        let other = indexed
            .take(line.len() - 1)
            .reduce(|largest, current| {
                if current.1 > largest.1 {
                    current
                } else {
                    largest
                }
            })
            .unwrap();
        let as_string = String::from_iter(vec![other.1, result.1]);
        as_string.parse::<u32>().unwrap()
    }
}

fn part_one(input: Vec<&str>) {
    let results = input
        .iter()
        .map(|x| process_one(*x).1)
        .collect::<Vec<char>>();
    println!("{:?}", results);
}
