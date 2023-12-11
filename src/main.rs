#![allow(dead_code)]

mod day1;
mod day2;

const AVG_SOLVE_TIME_LIMIT: u32 = 100_000;

fn main() {
    let res = day2::solve();
    println!("=== RESULT ===");
    println!("{res:?}");

    #[cfg(not(debug_assertions))]
    benchmark();
}

fn benchmark() {
    let mut solve_t = std::time::Duration::from_secs(0);
    let mut coeff = 0;
    loop {
        if coeff > AVG_SOLVE_TIME_LIMIT {
            println!("\n");
            break;
        }

        let tstart = std::time::Instant::now();
        day2::solve();
        let elapsed = tstart.elapsed();

        solve_t = ((solve_t * coeff) + elapsed) / (coeff + 1);
        coeff += 1;
        if coeff % (AVG_SOLVE_TIME_LIMIT / 1000) == 0 {
            print!("\rAverage elapsed time: {solve_t:?} ({coeff} runs)");
        }
    }
}
