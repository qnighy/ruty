pub(crate) fn get_many_mut<T, const N: usize>(
    mut s: &mut [T],
    indices: [usize; N],
) -> Result<[&mut T; N], GetManyError> {
    let mut index_map = [(0, 0); N];
    for j in 0..N {
        index_map[j] = (indices[j], j);
    }
    index_map.sort_unstable_by(|a, b| a.0.cmp(&b.0));
    for i in 1..N {
        if index_map[i - 1].0 == index_map[i].0 {
            return Err(GetManyError(()));
        }
    }
    if N > 0 && index_map[N - 1].0 >= s.len() {
        return Err(GetManyError(()));
    }
    let mut refs: [Option<&mut T>; N] = indices.map(|_| None);
    for (i, j) in index_map.into_iter().rev() {
        let (new_s, rest) = { s }.split_at_mut(i);
        refs[j] = Some(&mut rest[0]);
        s = new_s;
    }
    Ok(refs.map(|r| r.unwrap()))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct GetManyError(());
impl std::fmt::Display for GetManyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "get_many_mut: index out of bounds or duplicate")
    }
}
impl std::error::Error for GetManyError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pick_mut() {
        let mut s = [10, 35, 23, 10, 2, 8, 7, 99, 0, 4, 1, 37];
        let indices = [5, 3, 0, 10, 1];
        let picked = get_many_mut(&mut s, indices).unwrap();
        assert_eq!(picked, [&mut 8, &mut 10, &mut 10, &mut 1, &mut 35]);

        *picked[0] = 100;
        *picked[1] = 101;
        *picked[2] = 102;
        *picked[3] = 103;
        *picked[4] = 104;
        assert_eq!(s, [102, 104, 23, 101, 2, 100, 7, 99, 0, 4, 103, 37]);
    }
}
