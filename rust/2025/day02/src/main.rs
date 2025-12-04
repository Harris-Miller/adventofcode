use itertools::Itertools;
use std::ops::RangeInclusive;

fn main() {
    let input = include_str!("../../../../inputs/2025/Day2/input.txt");

    part_one(input);
    part_two(input);
}

fn create_ranges(input: &str) -> impl Iterator<Item = RangeInclusive<u64>> {
    input
        .trim()
        .split(",")
        .map(|p| {
            let (s, e) = p.split_once("-").unwrap();
            (s.parse::<u64>().unwrap(), e.parse::<u64>().unwrap())
        })
        .map(|(start, end)| start..=end)
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

fn part_one(input: &str) {
    let results = create_ranges(input).flat_map(process_one);
    let total: u64 = results.sum();
    println!("{:?}", total);
}

fn does_repeat(id: &String, by: usize) -> bool {
    id.chars()
        .chunks(by)
        .into_iter()
        .map(|c| c.collect::<String>())
        .all_equal()
}

fn determine_splits(id: &String) -> Vec<usize> {
    let len = id.len();
    let half_len = len / 2;
    (1..=half_len)
        .filter(|&n| (len % n) == 0)
        .collect::<Vec<usize>>()
}

fn process_two(range: RangeInclusive<u64>) -> Vec<u64> {
    range
        .filter(|val| {
            let as_str = val.to_string();
            let splits = determine_splits(&as_str);
            splits.iter().any(|by| does_repeat(&as_str, *by))
        })
        .collect::<Vec<u64>>()
}

fn part_two(input: &str) {
    let results = create_ranges(input).flat_map(process_two);
    let total: u64 = results.sum();
    println!("{:?}", total);
}
