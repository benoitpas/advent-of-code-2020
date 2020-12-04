object Day4 {

  import scala.io.Source

  val smallInput = """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
  hcl:#cfa07d byr:1929
  
hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in""".split("\n").toIndexedSeq
   
  val fullInput = Source.fromResource("day4-input.txt").getLines.to(LazyList)

  def extractPassportStrings(input: Seq[String]) = 
    input.foldRight(List[String]())(((s,l) => (s.trim,l) match {
      case ("", List()) => l 
      case ("", ""::_) => l
      case ("", _::_ ) => ""::l
      case (s, List()) => s::l 
      case (s, h::t) => (s+ " " + h)::t
    }))

  def passportStringToMap(s: String) = s.split(" ").flatMap(
    _.split(":").toList match {
      case k::v::_ => List((k,v))
      case _ => List()
  }).toMap

  val northPoleCredentialKeys = Set("ecl","eyr","hcl","byr","iyr","hgt","pid")
  val passportKeys = northPoleCredentialKeys + "cid"
  
  type Passport = Map[String,String]
  def checkPassport1(p:Passport) = 
    p.keySet == northPoleCredentialKeys || p.keySet == passportKeys

  def countPassport(passports: List[Passport], check:((Passport)=> Boolean)) =
    passports.filter(check).length

  val passports1 = extractPassportStrings(smallInput).map(passportStringToMap)
  val passports2 = extractPassportStrings(fullInput).map(passportStringToMap)

  def main(args: Array[String]): Unit = {
    println("Part1=" + countPassport(passports2, checkPassport1))
  }
}