use std::process::Command;

fn check_expanded(test_name: &str) -> anyhow::Result<()> {
    let cargo_expand = Command::new("cargo").args(&["expand", "--test", test_name]).output()?;
    assert!(cargo_expand.status.success());
    let expanded_str = String::from_utf8(cargo_expand.stdout)?;
    let should_be = std::fs::read_to_string(format!("tests/expanded/{test_name}.rs"))?;
    assert_eq!(expanded_str, should_be);
    Ok(())
}

#[test]
fn test_expand() -> anyhow::Result<()> {
    check_expanded("nest_complex")?;
    check_expanded("flatten_complex")?;
    Ok(())
}
