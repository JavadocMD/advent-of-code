package aoc2020

import utest._

object Day19Test extends TestSuite:
  import Day19._

  val tests = Tests {
    "match single char rules" - {
      val rules = Map("0" -> A)
      check("a", rules) ==> true
      check("aa", rules) ==> false
      check("b", rules) ==> false
      check("ba", rules) ==> false
      check("aba", rules) ==> false
    }

    "match or rules" - {
      val rules = Map(
        "0" -> Or(Sequence("1"), Sequence("2")),
        "1" -> A,
        "2" -> B
      )
      check("a", rules) ==> true
      check("b", rules) ==> true
      check("ab", rules) ==> false
      check("aa", rules) ==> false
      check("bb", rules) ==> false
      check("ba", rules) ==> false
    }

    "match seq rules" - {
      val rules = Map(
        "0" -> Sequence("1", "2", "1"),
        "1" -> A,
        "2" -> B
      )
      check("aba", rules) ==> true
      check("abb", rules) ==> false
      check("ab", rules) ==> false
      check("bba", rules) ==> false
      check("bb", rules) ==> false
    }

    "match example" - {
      val rules = Map(
        "0" -> Sequence("4", "1", "5"),
        "1" -> Or(Sequence("2", "3"), Sequence("3", "2")),
        "2" -> Or(Sequence("4", "4"), Sequence("5", "5")),
        "3" -> Or(Sequence("4", "5"), Sequence("5", "4")),
        "4" -> A,
        "5" -> B
      )
      check("aaaabb", rules) ==> true
      check("aaabab", rules) ==> true
      check("abbabb", rules) ==> true
      check("abbbab", rules) ==> true
      check("aabaab", rules) ==> true
      check("aabbbb", rules) ==> true
      check("abaaab", rules) ==> true
      check("ababbb", rules) ==> true
      check("aaaaaa", rules) ==> false
      check("bbbbbb", rules) ==> false
    }

    "parse example" - {
      val rules = """0: 4 1 5
        |1: 2 3 | 3 2
        |2: 4 4 | 5 5
        |3: 4 5 | 5 4
        |4: "a"
        |5: "b"""".stripMargin.linesIterator.toArray

      val expected = Map(
        "0" -> Sequence("4", "1", "5"),
        "1" -> Or(Sequence("2", "3"), Sequence("3", "2")),
        "2" -> Or(Sequence("4", "4"), Sequence("5", "5")),
        "3" -> Or(Sequence("4", "5"), Sequence("5", "4")),
        "4" -> A,
        "5" -> B
      )

      parseRules(rules) ==> expected
    }

    "handle rules with cycles" - {
      val input = """42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: "a"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: "b"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba""".linesIterator.toArray

      val Array(rules, messages) = toChunks(input)

      val ruleMap = parseRules(rules)
        .updated("8", Or(Sequence("42"), Sequence("42", "8")))
        .updated("11", Or(Sequence("42", "31"), Sequence("42", "11", "31")))

      check("bbabbbbaabaabba", ruleMap) ==> true
      check("babbbbaabbbbbabbbbbbaabaaabaaa", ruleMap) ==> true
      check("aaabbbbbbaaaabaababaabababbabaaabbababababaaa", ruleMap) ==> true
      check("bbbbbbbaaaabbbbaaabbabaaa", ruleMap) ==> true
      check("bbbababbbbaaaaaaaabbababaaababaabab", ruleMap) ==> true
      check("ababaaaaaabaaab", ruleMap) ==> true
      check("ababaaaaabbbaba", ruleMap) ==> true
      check("baabbaaaabbaaaababbaababb", ruleMap) ==> true
      check("abbbbabbbbaaaababbbbbbaaaababb", ruleMap) ==> true
      check("aaaaabbaabaaaaababaa", ruleMap) ==> true
      check("aaaabbaabbaaaaaaabbbabbbaaabbaabaaa", ruleMap) ==> true
      check("aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba", ruleMap) ==> true

      part1(rules, messages) ==> 3
      part2(rules, messages) ==> 12
    }
  }
