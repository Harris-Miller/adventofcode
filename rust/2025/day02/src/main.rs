use std::ops::RangeInclusive;

fn main() {
    let input = include_str!("../../../../inputs/2025/Day2/input.txt");

    part_one(input);
}

fn part_one(input: &str) {
    let ranges = input.trim().split(",").map(|p| {
        let (s, e) = p.split_once("-").unwrap();
        (s.parse::<u64>().unwrap(), e.parse::<u64>().unwrap())
    });

    let vectors = ranges.map(|(start, end)| start..=end);
    let results = vectors.flat_map(process_one);
    let total: u64 = results.sum();
    println!("{:?}", total);
}

fn process_one(range: RangeInclusive<u64>) -> Vec<u64> {
    range
        .map(|val| val.to_string())
        .filter(|val| val.len() % 2 == 0)
        .filter(|val| {
            let mid = val.len() / 2;
            let (first, second) = val.split_at(mid);
            first == second
        })
        .map(|val| val.parse::<u64>().unwrap())
        .collect()
}
