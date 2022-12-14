use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar/riscv.pest"]
struct RiscvParser;

pub fn parse(src: &str) {
    let pairs = RiscvParser::parse(Rule::source, src).unwrap_or_else(|e| panic!("{}", e));

    println!("{}", pairs.clone());
    // println!("Debug
    // start-----------------------------------------------------------------");
    // for pair in pairs.clone() {
    //     error_message(pair);
    // }
    // println!("Debug
    // end------------------------------------------------------------------");

    let mut nest_times: usize = 0;

    for pair in pairs {
        match pair.as_rule() {
            Rule::label => {
                println!("label: {}", pair.as_str());
                nest_times = 1;
            }
            Rule::directive => parse_directive(pair, nest_times),
            Rule::instruction => parse_instruction(pair, nest_times),
            Rule::comment => println!("comment: {}", pair.as_str()),
            Rule::EOI => break,
            _ => error_message(pair),
        }
    }

    println!("nest_times: {}", nest_times);
}

fn print_offset(nest_times: usize) {
    let offset_str = String::from_utf8(vec![b' '; 4 * nest_times]).unwrap();
    print!("{offset_str}");
}

///
/// - expect `.section .text .text1 .text-1`
fn parse_directive(pair: pest::iterators::Pair<Rule>, nest_times: usize) {
    let mut pseudo_op = "";
    let mut operands = Vec::new();

    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::pseudo_op => {
                pseudo_op = pair.as_str();
            }
            Rule::operand => {
                operands.push(pair.as_str());
            }
            _ => error_message(pair),
        }
    }

    if !pseudo_op.is_empty() || !operands.is_empty() {
        print_offset(nest_times);
        println!("pseudo:{}, arg:{:?};", pseudo_op, operands);
    }
}

/// - expect `addi a1, a1, 1`
fn parse_instruction(pair: pest::iterators::Pair<Rule>, nest_times: usize) {
    let mut variable_name = "";
    let mut operands = Vec::new();

    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::mnemonic => {
                variable_name = pair.as_str().trim();
            }
            Rule::operand => {
                operands.push(pair.as_str());
            }
            _ => error_message(pair),
        }
    }

    if !variable_name.is_empty() || !operands.is_empty() {
        print_offset(nest_times);
        println!("op: {}, args: {:?};", variable_name, operands);
    }
}

fn error_message(pair: pest::iterators::Pair<Rule>) {
    println!("--------------------------------------------------------");
    println!("Unreachable!!!!");
    println!("Rule:    {:?}", pair.as_rule());
    println!("Span:    {:?}", pair.as_span());
    println!("Text:    {}", pair.as_str());
    println!("--------------------------------------------------------");
}

mod test {

    #[test]
    fn parse_testfile() {
        use super::*;

        let unparsed_text = include_str!("../../examples/entry.S");
        parse(unparsed_text);

        // println!("{:?}", parsed_text);
    }
}
