fn main() {
    let input = include_str!("../../../../inputs/2025/Day2/sample.txt");

    part_one(input);
}

fn part_one(input: &str) {
    let ranges = input.trim().split(",").map(|p| {
        let (s, e) = p.split_once("-").unwrap();
        (s.parse::<u32>().unwrap(), e.parse::<u32>().unwrap())
    });

    let vectors = ranges.map(|(start, end)| {
        let what = start..=end;
        let things: Vec<u32> = what.collect();
        things
    });

    println!("{:?}", vectors.collect::<Vec<Vec<u32>>>());
}
