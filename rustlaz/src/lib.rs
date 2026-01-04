use std::os::raw::c_int;

#[unsafe(no_mangle)]
pub extern "C" fn add_numbers(a: i32, b: i32) -> i32 {
    a + b
}

#[repr(C)]
#[derive(Debug, PartialEq)]
pub struct Point {
    pub x: c_int,
    pub y: c_int,
}

#[unsafe(no_mangle)]
pub extern "C" fn move_point(p: Point, dx: c_int, dy: c_int) -> Point {
    Point {
        x: p.x + dx,
        y: p.y + dy,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_numbers_test() {
        let result = add_numbers(2, 2);
        assert_eq!(result, 4);
    }

    #[test]
    fn point_test() {
        let p = Point { x: 1, y: 1 };
        let p2 = move_point(p, 1, 2);
        assert_eq!(p2, Point { x: 2, y: 3 })
    }
}
