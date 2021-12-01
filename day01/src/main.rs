use rayon::prelude::*;
use std::error::Error;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

type Result<T> = ::std::result::Result<T, Box<dyn Error>>;

fn main() -> Result<()> {
    let input = input()?;

    println!("part1 answer: {}", part1(&input));
    println!("part2 answer: {}", part2(&input));

    Ok(())
}

fn part1(measurements: &[i64]) -> i32 {
    measurements
        .par_windows(2)
        .map(|pair| (pair[1] > pair[0]) as i32)
        .sum()
}

fn part2(measurements: &[i64]) -> i32 {
    measurements
        .par_windows(4)
        .map(|tup| (tup[3] > tup[0]) as i32)
        .sum()
}

// TODO: The following two functions, read_lines and input, are way
// too ugly. It must be possible to write them nicer

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

fn input() -> Result<Vec<i64>> {
    let lines = read_lines("input.txt");
    Ok(lines?.map(|line| line.unwrap().parse().unwrap()).collect())
}
