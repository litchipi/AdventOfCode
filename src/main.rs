#![allow(dead_code)]

mod day1;
mod day2;

const AVG_SOLVE_TIME_LIMIT: u32 = 200_000;

fn main() {
    let f = day2::solve;
    let res = f();
    println!("=== RESULT ===");
    println!("{res:?}");

    #[cfg(not(debug_assertions))]
    benchmark(|| { let _ = f(); });
}

fn benchmark<F: FnOnce() + Copy>(f: F) {
    let mut solve_t = std::time::Duration::from_secs(0);

    // Warmup
    for _ in 0..(AVG_SOLVE_TIME_LIMIT / 1_000) {
        f();
    }

    for coeff in 0..AVG_SOLVE_TIME_LIMIT {
        let tstart = std::time::Instant::now();
        f();
        let elapsed = tstart.elapsed();

        solve_t = ((solve_t * coeff) + elapsed) / (coeff + 1);
        if coeff % (AVG_SOLVE_TIME_LIMIT / 1000) == 0 {
            print!("\rAverage elapsed time: {solve_t:?} ({coeff} runs)");
        }
    }
    println!("\n");
}
