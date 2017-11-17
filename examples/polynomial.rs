extern crate imp;

use imp::language::*;
use imp::interpreter::*;
use imp::compiled::*;

pub fn polynomial(prepared: &Prepared) -> (Vec<i64>, Vec<i64>, Vec<i64>) {
    let mut results_x = vec![];
    let mut results_y = vec![];
    let mut results_z = vec![];

    let x0 = prepared.indexes[0].columns[0].as_integers();
    let x1 = prepared.indexes[0].columns[1].as_integers();
    let y0 = prepared.indexes[1].columns[0].as_integers();
    let y1 = prepared.indexes[1].columns[1].as_integers();

    let x_range = (0, x0.len());
    let y_range = (0, y0.len());

    join2(x0, y0, x_range, y_range, |x_range, y_range| {
        join1(x1, x_range, |x_range| {
            join1(y1, y_range, |y_range| {
                let x = x1[x_range.0];
                let y = y1[y_range.0];
                let z = (x * x) + (y * y) + (3 * x * y);
                results_x.push(x);
                results_y.push(y);
                results_z.push(z);
            });
        });
    });

    (results_x, results_y, results_z)
}

fn main() {
    let db = polynomial_db();
    let block = plan(&block_ast(POLYNOMIAL).unwrap()).unwrap();
    let prepared = prepare_block(&block, &db).unwrap();
    polynomial(&prepared);
}
