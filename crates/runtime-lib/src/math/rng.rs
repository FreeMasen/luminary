//! This module provides PRNG to generate a string of random numbers

use core::sync::atomic::Ordering;
use std::sync::atomic::AtomicI64;

// According to `man srand` if not called the default is 1
static SEED: AtomicI64 = AtomicI64::new(1);
static X: AtomicI64 = AtomicI64::new(0);
static W: AtomicI64 = AtomicI64::new(0);

pub fn seed_random(seed: i64) {
    SEED.store(seed, Ordering::SeqCst);
    X.store(0, Ordering::SeqCst);
    W.store(0, Ordering::SeqCst);
}

pub fn get_random() -> i64 {
    let s = SEED.load(Ordering::Relaxed);
    let old_w = W.load(Ordering::Relaxed);
    let new_w = old_w.wrapping_add(s);
    let old_x = X.load(Ordering::Relaxed);
    let new_x = old_w.wrapping_mul(old_x).wrapping_add(new_w);
    let new_x = new_x >> 32 | new_x << 32;
    W.store(new_w, Ordering::SeqCst);
    X.store(new_x, Ordering::SeqCst);
    new_x
}
