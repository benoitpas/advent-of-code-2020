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

  def extractBlocks(input: Seq[String]) = 
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

  val passports1 = extractBlocks(smallInput).map(passportStringToMap)
  val passports2 = extractBlocks(fullInput).map(passportStringToMap)

  def checkYear(value:String, min: Int, max: Int) = {
    val valueRegex = "\\d{4}".r
    value match {
      case valueRegex() => min <= value.toInt && value.toInt <= max
      case _ => false
    }
  }

  def checkHeight(value:String) = {
    val valueRegex = "(\\d+)(in|cm)".r
    valueRegex.findFirstMatchIn(value) match {
      case Some(m) => {
        val height = m.group(1).toInt
        val unit = m.group(2)
        (unit == "cm" && 150 <= height && height <= 193)
        || (unit =="in" && 59 <= height && height <= 76)
      }
      case _ => false
    }
  }

  def checkHairColor(value:String) = {
    val valueRegex = "#(\\d|[a-f]){6}".r
    valueRegex.matches(value)
  }

  def CheckEyeColor(value:String) = {
    val valueRegex = "(amb)|(blu)|(brn)|(gry)|(grn)|(hzl)|(oth)".r
    valueRegex.matches(value)
  }

  def CheckPassportID(value:String) = {
    val valueRegex = "\\d{9}".r
    valueRegex.matches(value)
  }


  def checkField(field:String, value: String) : Boolean = field match {
    case "byr" => checkYear(value,1920,2002)
    case "iyr" => checkYear(value,2010,2020)
    case "eyr" => checkYear(value,2020,2030)
    case "hgt" => checkHeight(value)
    case "hcl" => checkHairColor(value)
    case "ecl" => CheckEyeColor(value)
    case "pid" => CheckPassportID(value)
    case _ => true
  }

  def checkPassport2(p:Passport) = 
    checkPassport1(p) && p.foldRight(true)((f,a) => a && checkField(f._1,f._2))

  def main(args: Array[String]): Unit = {
    println("Part1=" + countPassport(passports2, checkPassport1))
    println("Part2=" + countPassport(passports2, checkPassport2))
  }
}