use language::*;
use interpreter::*;

fn smaller((a_lo, a_hi): (usize, usize), (b_lo, b_hi): (usize, usize)) -> bool {
    (a_hi - a_lo) < (b_hi - b_lo)
}

fn narrow<T1, T2, F>(
    values: &[T1],
    (lo, hi): (usize, usize),
    value: &T2,
    mut f: F
) where
    T1: ::std::borrow::Borrow<T2>,
    T2: Ord + ?Sized,
    F: FnMut((usize, usize)) {
    let lo = gallop_le_inner(values, lo, hi, value);
    let hi = gallop_leq_inner(values, lo, hi, value);
    if lo < hi {
        f((lo, hi))
    }
}

fn join1<T, F>(
    a: &[T],
    (mut a_lo, a_hi): (usize, usize),
    mut f: F
) where
    T: Ord,
    F: FnMut((usize, usize)),
{
    while a_lo < a_hi {
        let value = &a[a_lo];
        let a_next_lo = gallop_leq_inner(a, a_lo, a_hi, value);
        f((a_lo, a_next_lo));
        a_lo = a_next_lo;
    }
}

fn join_inner2<T, F>(
    a: &[T],
    b: &[T],
    a_range: (usize, usize),
    b_range: (usize, usize),
    mut f: F,
) where
    T: Ord,
    F: FnMut((usize, usize), (usize, usize)),
{
    let mut b_lo = b_range.0;
    join1(a, a_range, |a_range| {
        let value = &a[a_range.0];
        narrow(b, (b_lo, b_range.1), value, |b_range| {
            f(a_range, b_range);
            b_lo = b_range.0;
        });
    });
}

fn join_inner3<T, F>(
    a: &[T],
    b: &[T],
    c: &[T],
    a_range: (usize, usize),
    b_range: (usize, usize),
    c_range: (usize, usize),
    mut f: F,
) where
    T: Ord,
    F: FnMut((usize, usize), (usize, usize), (usize, usize)),
{
    let mut c_lo = c_range.0;
    join_inner2(a, b, a_range, b_range, |a_range, b_range| {
        let value = &a[a_range.0];
        narrow(c, (c_lo, c_range.1), value, |c_range| {
            f(a_range, b_range, c_range);
            c_lo = c_range.0;
        });
    });
}

fn join_inner4<T, F>(
    a: &[T],
    b: &[T],
    c: &[T],
    d: &[T],
    a_range: (usize, usize),
    b_range: (usize, usize),
    c_range: (usize, usize),
    d_range: (usize, usize),
    mut f: F,
) where
    T: Ord,
    F: FnMut((usize, usize), (usize, usize), (usize, usize), (usize, usize)),
{
    let mut d_lo = d_range.0;
    join_inner3(a, b, c, a_range, b_range, c_range, |a_range, b_range, c_range| {
        let value = &a[a_range.0];
        narrow(d, (d_lo, d_range.1), value, |d_range| {
            f(a_range, b_range, c_range, d_range);
            d_lo = d_range.0;
        });
    });
}

fn join2<T, F>(
    a: &[T],
    b: &[T],
    a_range: (usize, usize),
    b_range: (usize, usize),
    mut f: F,
) where
    T: Ord,
    F: FnMut((usize, usize), (usize, usize)),
{
    if smaller(a_range, b_range) {
        join_inner2(a, b, a_range, b_range, |a_range, b_range| {
            f(a_range, b_range);
        });
    } else {
        join_inner2(b, a, b_range, a_range, |b_range, a_range| {
            f(a_range, b_range);
        });
    }
}

fn join3<T, F>(
    a: &[T],
    b: &[T],
    c: &[T],
    a_range: (usize, usize),
    b_range: (usize, usize),
    c_range: (usize, usize),
    mut f: F,
) where
    T: Ord,
    F: FnMut((usize, usize), (usize, usize), (usize, usize)),
{
    if smaller(a_range, b_range) && smaller(a_range, c_range) {
        join_inner3(a, b, c, a_range, b_range, c_range, |a_range, b_range, c_range| {
            f(a_range, b_range, c_range);
        });
    } else if smaller(b_range, a_range) && smaller(b_range, c_range) {
        join_inner3(b, a, c, b_range, a_range, c_range, |b_range, a_range, c_range| {
            f(a_range, b_range, c_range);
        });
    } else {
        join_inner3(c, b, a, c_range, b_range, a_range, |c_range, b_range, a_range| {
            f(c_range, b_range, a_range);
        });
    }
}

fn join4<T, F>(
    a: &[T],
    b: &[T],
    c: &[T],
    d: &[T],
    a_range: (usize, usize),
    b_range: (usize, usize),
    c_range: (usize, usize),
    d_range: (usize, usize),
    mut f: F,
) where
    T: Ord,
    F: FnMut((usize, usize), (usize, usize), (usize, usize), (usize, usize)),
{
    if smaller(a_range, b_range) && smaller(a_range, c_range) && smaller(a_range, d_range) {
        join_inner4(a, b, c, d, a_range, b_range, c_range, d_range, |a_range, b_range, c_range, d_range| {
            f(a_range, b_range, c_range, d_range);
        });
    } else if smaller(b_range, a_range) && smaller(b_range, c_range) && smaller(b_range, d_range) {
        join_inner4(b, a, c, d, b_range, a_range, c_range, d_range, |b_range, a_range, c_range, d_range| {
            f(b_range, a_range, c_range, d_range);
        });
    } else if smaller(c_range, a_range) && smaller(c_range, b_range) && smaller(c_range, d_range){
        join_inner4(c, b, a, d, c_range, b_range, a_range, d_range, |c_range, b_range, a_range, d_range| {
            f(a_range, b_range, c_range, d_range);
        });
    } else {
        join_inner4(d, b, c, a, d_range, b_range, c_range, a_range, |d_range, b_range, c_range, a_range| {
            f(a_range, b_range, c_range, d_range);
        });
    }
}


pub fn q1a(
    prepared: &Prepared,
) -> (Vec<i64>, Vec<i64>, Vec<i64>, Vec<String>, Vec<i64>, Vec<i64>, Vec<i64>, Vec<String>) {
    let mut results_it = vec![];
    let mut results_mi = vec![];
    let mut results_t = vec![];
    let mut results_title = vec![];
    let mut results_production_year = vec![];
    let mut results_mc = vec![];
    let mut results_ct = vec![];
    let mut results_note = vec![];

    let info_type_info0 = prepared.indexes[0].columns[0].as_integers();
    let info_type_info1 = prepared.indexes[0].columns[1].as_strings();
    let movie_info_idx_info_type0 = prepared.indexes[1].columns[0].as_integers();
    let movie_info_idx_info_type1 = prepared.indexes[1].columns[1].as_integers();
    let movie_info_idx_movie0 = prepared.indexes[2].columns[0].as_integers();
    let movie_info_idx_movie1 = prepared.indexes[2].columns[1].as_integers();
    let title_title0 = prepared.indexes[3].columns[0].as_integers();
    let title_title1 = prepared.indexes[3].columns[1].as_strings();
    let title_production_year0 = prepared.indexes[4].columns[0].as_integers();
    let title_production_year1 = prepared.indexes[4].columns[1].as_integers();
    let movie_companies_movie0 = prepared.indexes[5].columns[0].as_integers();
    let movie_companies_movie1 = prepared.indexes[5].columns[1].as_integers();
    let movie_companies_company_type0 = prepared.indexes[6].columns[0].as_integers();
    let movie_companies_company_type1 = prepared.indexes[6].columns[1].as_integers();
    let company_type_kind0 = prepared.indexes[7].columns[0].as_integers();
    let company_type_kind1 = prepared.indexes[7].columns[1].as_strings();
    let movie_companies_note0 = prepared.indexes[8].columns[0].as_integers();
    let movie_companies_note1 = prepared.indexes[8].columns[1].as_strings();

    let info_type_info_range = (0, info_type_info1.len());
    let movie_info_idx_info_type_range = (0, movie_info_idx_info_type1.len());
    let movie_info_idx_movie_range = (0, movie_info_idx_movie1.len());
    let title_title_range = (0, title_title1.len());
    let title_production_year_range = (0, title_production_year1.len());
    let movie_companies_movie_range = (0, movie_companies_movie1.len());
    let movie_companies_company_type_range = (0, movie_companies_company_type1.len());
    let company_type_kind_range = (0, company_type_kind1.len());
    let movie_companies_note_range = (0, movie_companies_note1.len());

    narrow(company_type_kind1, company_type_kind_range, "production companies", |company_type_kind_range| {
        narrow(info_type_info1, info_type_info_range, "top 250 rank", |info_type_info_range| {
            // it
            join2(info_type_info0, movie_info_idx_info_type1, info_type_info_range, movie_info_idx_info_type_range, |info_type_info_range, movie_info_idx_info_type_range| {
                // mi
                join2(movie_info_idx_info_type0, movie_info_idx_movie0, movie_info_idx_info_type_range, movie_info_idx_movie_range, |movie_info_idx_info_type_range, movie_info_idx_movie_range| {
                    // t
                    join4(movie_info_idx_movie1, title_title0, title_production_year0, movie_companies_movie1, movie_info_idx_movie_range, title_title_range, title_production_year_range, movie_companies_movie_range, |movie_info_idx_movie_range, title_title_range, title_production_year_range, movie_companies_movie_range| {
                        // title
                        join1(title_title1, title_title_range, |title_title_range| {
                            // production_year
                            join1(title_production_year1, title_production_year_range, |title_production_year_range| {
                                // mc
                                join3(movie_companies_movie0, movie_companies_company_type0, movie_companies_note0, movie_companies_movie_range, movie_companies_company_type_range, movie_companies_note_range, |movie_companies_movie_range, movie_companies_company_type_range, movie_companies_note_range| {
                                    // ct
                                    join2(movie_companies_company_type1, company_type_kind0, movie_companies_company_type_range, company_type_kind_range, |movie_companies_company_type_range, _company_type_kind_range| {
                                        // note
                                        join1(movie_companies_note1, movie_companies_note_range, |movie_companies_note_range| {
                                            let note = &*movie_companies_note1[movie_companies_note_range.0];
                                            if !note.contains("(as Metro-Goldwyn-Mayer Pictures)") && (note.contains("(co-production)") || note.contains("(presents)")) {
                                                results_it.push(info_type_info0[info_type_info_range.0]);
                                                results_mi.push(movie_info_idx_info_type0[movie_info_idx_info_type_range.0]);
                                                results_t.push(movie_info_idx_movie1[movie_info_idx_movie_range.0]);
                                                results_title.push(title_title1[title_title_range.0].clone());
                                                results_production_year.push(title_production_year1[title_production_year_range.0]);
                                                results_mc.push(movie_companies_movie0[movie_companies_movie_range.0]);
                                                results_ct.push(movie_companies_company_type1[movie_companies_company_type_range.0]);
                                                results_note.push(movie_companies_note1[movie_companies_note_range.0].clone());
                                            }
                                        });
                                    });
                                });
                            });
                        });
                    });
                });
            });
        });
    });

    (
        results_it,
        results_mi,
        results_t,
        results_title,
        results_production_year,
        results_mc,
        results_ct,
        results_note,
    )
}

pub fn q2c(
    prepared: &Prepared,
) -> (Vec<i64>, Vec<i64>, Vec<i64>, Vec<String>, Vec<i64>, Vec<i64>) {

    let mut results_k = vec![];
    let mut results_mk = vec![];
    let mut results_t = vec![];
    let mut results_title = vec![];
    let mut results_mc = vec![];
    let mut results_cn = vec![];

    let keyword_keyword0 = &prepared.indexes[0].columns[0].as_integers();
    let keyword_keyword1 = &prepared.indexes[0].columns[1].as_strings();
    let movie_keyword_keyword0 = &prepared.indexes[1].columns[0].as_integers();
    let movie_keyword_keyword1 = &prepared.indexes[1].columns[1].as_integers();
    let movie_keyword_movie0 = &prepared.indexes[2].columns[0].as_integers();
    let movie_keyword_movie1 = &prepared.indexes[2].columns[1].as_integers();
    let title_title0 = &prepared.indexes[3].columns[0].as_integers();
    let title_title1 = &prepared.indexes[3].columns[1].as_strings();
    let movie_companies_movie0 = &prepared.indexes[4].columns[0].as_integers();
    let movie_companies_movie1 = &prepared.indexes[4].columns[1].as_integers();
    let movie_companies_company0 = &prepared.indexes[5].columns[0].as_integers();
    let movie_companies_company1 = &prepared.indexes[5].columns[1].as_integers();
    let company_name_country_code0 = &prepared.indexes[6].columns[0].as_integers();
    let company_name_country_code1 = &prepared.indexes[6].columns[1].as_strings();

    let keyword_keyword_range = (0, keyword_keyword0.len());
    let movie_keyword_keyword_range = (0, movie_keyword_keyword0.len());
    let movie_keyword_movie_range = (0, movie_keyword_movie0.len());
    let title_title_range = (0, title_title0.len());
    let movie_companies_movie_range = (0, movie_companies_movie0.len());
    let movie_companies_company_range = (0, movie_companies_company0.len());
    let company_name_country_code_range = (0, company_name_country_code0.len());

    narrow(company_name_country_code1, company_name_country_code_range, "[sm]", |company_name_country_code_range| {
        narrow(keyword_keyword1, keyword_keyword_range, "character-name-in-title", |keyword_keyword_range| {
            // k
            join2(keyword_keyword0, movie_keyword_keyword1, keyword_keyword_range, movie_keyword_keyword_range, |keyword_keyword_range, movie_keyword_keyword_range| {
                // mk
                join2(movie_keyword_keyword0, movie_keyword_movie0, movie_keyword_keyword_range, movie_keyword_movie_range, |movie_keyword_keyword_range, movie_keyword_movie_range| {
                    // t
                    join3(movie_keyword_movie1, title_title0, movie_companies_movie1, movie_keyword_movie_range, title_title_range, movie_companies_movie_range, |movie_keyword_movie_range, title_title_range, movie_companies_movie_range| {
                        // title
                        join1(title_title1, title_title_range, |title_title_range| {
                            // mc
                            join2(movie_companies_movie0, movie_companies_company0, movie_companies_movie_range, movie_companies_company_range, |movie_companies_movie_range, movie_companies_company_range| {
                                // cn
                                join2(movie_companies_company1, company_name_country_code0, movie_companies_company_range, company_name_country_code_range, |movie_companies_company_range, _company_name_country_code_range| {
                                    results_k.push(keyword_keyword0[keyword_keyword_range.0]);
                                    results_mk.push(movie_keyword_keyword0[movie_keyword_keyword_range.0]);
                                    results_t.push(movie_keyword_movie1[movie_keyword_movie_range.0]);
                                    results_title.push(title_title1[title_title_range.0].clone());
                                    results_mc.push(movie_companies_movie0[movie_companies_movie_range.0]);
                                    results_cn.push(movie_companies_company1[movie_companies_company_range.0]);
                                });
                            });
                        });
                    });
                });
            });
        });
    });

    (
        results_k,
        results_mk,
        results_t,
        results_title,
        results_mc,
        results_cn,
    )
}

pub fn polynomial_db() -> DB {
    let x = Relation{columns: vec![Values::Integer((0..1000000).collect()), Values::Integer((0..1000000).collect())]};
    let y = Relation{columns: vec![Values::Integer((0..1000000).collect()), Values::Integer((0..1000000).rev().collect())]};
    DB{relations: vec![("x".to_owned(), x), ("y".to_owned(), y)].into_iter().collect()}
}

pub static POLYNOMIAL: &'static str = "
x(i, x)
y(i, y)
((x * x) + (y * y) + (3 * x * y)) = z
";

pub static POLYNOMIAL_MAGIC: &'static str = "
x(i, x)
y(i, y)
magic(x, y) = z
";

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

    (
        results_x,
        results_y,
        results_z,
    )
}

pub fn polynomial_baseline(prepared: &Prepared) -> (Vec<i64>, Vec<i64>, Vec<i64>) {
    let mut results_x = vec![];
    let mut results_y = vec![];
    let mut results_z = vec![];
    
    let _x0 = prepared.indexes[0].columns[0].as_integers();
    let x1 = prepared.indexes[0].columns[1].as_integers();
    let _y0 = prepared.indexes[1].columns[0].as_integers();
    let y1 = prepared.indexes[1].columns[1].as_integers();

    for (&x, &y) in x1.iter().zip(y1.iter()) {
        let z = (x * x) + (y * y) + (3 * x * y);
        results_x.push(x);
        results_y.push(y);
        results_z.push(z);
    }

    (
        results_x,
        results_y,
        results_z,
    )
}

pub fn polynomial_intermediate(prepared: &Prepared) -> (Vec<i64>, Vec<i64>, Vec<i64>) {
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
                results_x.push(x);
                results_y.push(y);
            });
        });
    });

    for (&x, &y) in x1.iter().zip(y1.iter()) {
        let z = (x * x) + (y * y) + (3 * x * y);
        results_z.push(z);
    }

    (
        results_x,
        results_y,
        results_z,
    )
}

pub fn polynomial_boxfn(block: &Block, prepared: &Prepared) -> (Vec<i64>, Vec<i64>, Values) {
    let mut results_x = vec![];
    let mut results_y = vec![];
    let mut results_z = Values::Integer(vec![]);
    
    let x0 = prepared.indexes[0].columns[0].as_integers();
    let x1 = prepared.indexes[0].columns[1].as_integers();
    let y0 = prepared.indexes[1].columns[0].as_integers();
    let y1 = prepared.indexes[1].columns[1].as_integers();

    let x_range = (0, x0.len());
    let y_range = (0, y0.len());

    let boxfn = match &block.constraints[3] {
        &Constraint::Apply(_, _, ref function) => function.compile(),
        _ => panic!(),
    };
    let mut variables: Vec<Value<'static>> = vec![Value::Integer(0), Value::Integer(0), Value::Integer(0)];
        

    join2(x0, y0, x_range, y_range, |x_range, y_range| {
        join1(x1, x_range, |x_range| {
            join1(y1, y_range, |y_range| {
                let x = x1[x_range.0];
                let y = y1[y_range.0];
                variables[1] = Value::Integer(x);
                variables[2] = Value::Integer(y);
                let z = boxfn(&*variables).unwrap();
                results_x.push(x);
                results_y.push(y);
                results_z.push(z);
            });
        });
    });

    (
        results_x,
        results_y,
        results_z,
    )
}

pub fn polynomial_fn(block: &Block, prepared: &Prepared) -> (Vec<i64>, Vec<i64>, Values) {
    let mut results_x = vec![];
    let mut results_y = vec![];
    let mut results_z = Values::Integer(vec![]);
    
    let x0 = prepared.indexes[0].columns[0].as_integers();
    let x1 = prepared.indexes[0].columns[1].as_integers();
    let y0 = prepared.indexes[1].columns[0].as_integers();
    let y1 = prepared.indexes[1].columns[1].as_integers();

    let x_range = (0, x0.len());
    let y_range = (0, y0.len());

    let function = match &block.constraints[3] {
        &Constraint::Apply(_, _, ref function) => function,
        _ => panic!(),
    };
    let mut variables: Vec<Value<'static>> = vec![Value::Integer(0), Value::Integer(0), Value::Integer(0)];
        

    join2(x0, y0, x_range, y_range, |x_range, y_range| {
        join1(x1, x_range, |x_range| {
            join1(y1, y_range, |y_range| {
                let x = x1[x_range.0];
                let y = y1[y_range.0];
                variables[1] = Value::Integer(x);
                variables[2] = Value::Integer(y);
                let z = function.apply(&*variables).unwrap();
                results_x.push(x);
                results_y.push(y);
                results_z.push(z);
            });
        });
    });

    (
        results_x,
        results_y,
        results_z,
    )
}
