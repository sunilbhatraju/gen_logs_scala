import scala.collection.mutable.Map
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import java.io._

class IPGenerator(var sessionCount: Int, var session_length: Int) {
  var sessions = Map[String, Int]()

  def get_ip = {
    session_gc
    sessionCreate
    val ip = sessions.keys.toSeq(Random.nextInt(sessions.size))
    sessions(ip) += 1
    ip
  }

  private def sessionCreate = {
    while(sessions.size < sessionCount) {
      sessions(randomIp) = 0
    }
  }

  private def session_gc = {
    for((ip, count) <- sessions) {
      if (count >= session_length) sessions.remove(ip)
    }
  }

  private def randomIp: String = {
    val random = Random
    var octets = ArrayBuffer[Int]()
    octets += random.nextInt(223) + 1
    (1 to 3).foreach { _ => octets += random.nextInt(255) }
    octets.mkString(".")
  }
}

class LogGenerator(val ipgen: IPGenerator, var messagesCount: Int = 1) {

  val PRODUCTS = Map()

  val REQUESTS = Map(
    "/departments" -> 40,
    "/department/*DEPARTMENT*/categories"-> 20,
    "/department/*DEPARTMENT*/products"-> 10,
    "/categories/*CATEGORY*/products"-> 5,
    "/product/*PRODUCT*"-> 10,
    "/add_to_cart/*PRODUCT*"-> 5,
    "/login"-> 5,
    "/logout"-> 2,
    "/checkout"-> 3,
    "/support"-> 1
  )

  val EXTENSIONS =  Map(
    "html" -> 40,
    "php" -> 30,
    "png" -> 15,
    "gif" -> 10,
    "css" -> 5
  )

  val RESPONSE_CODES = Map(
    "200" -> 92,
    "404" -> 5,
    "503" -> 3
  )

  val USER_AGENTS = Map(
    "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/35.0.1916.153 Safari/537.36"-> 11,
    "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:30.0) Gecko/20100101 Firefox/30.0"-> 6,
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/35.0.1916.153 Safari/537.36"-> 5,
    "Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/35.0.1916.153 Safari/537.36"-> 4,
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_4) AppleWebKit/537.77.4 (KHTML, like Gecko) Version/7.0.5 Safari/537.77.4"-> 4,
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/35.0.1916.153 Safari/537.36"-> 3,
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:30.0) Gecko/20100101 Firefox/30.0"-> 3,
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_3) AppleWebKit/537.76.4 (KHTML, like Gecko) Version/7.0.4 Safari/537.76.4"-> 3,
    "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/35.0.1916.153 Safari/537.36"-> 2,
    "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/36.0.1985.125 Safari/537.36"-> 2,
    "Mozilla/5.0 (Windows NT 6.3; WOW64; rv:30.0) Gecko/20100101 Firefox/30.0"-> 2,
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/36.0.1985.125 Safari/537.36"-> 2,
    "Mozilla/5.0 (Windows NT 6.1; WOW64; Trident/7.0; rv:11.0) like Gecko"-> 2,
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/35.0.1916.153 Safari/537.36"-> 2,
    "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0"-> 1,
    "Mozilla/5.0 (Windows NT 6.1; rv:30.0) Gecko/20100101 Firefox/30.0"-> 1
  )

  val DEPARTMENTS = Map()

  val CATEGORIES = Map()

  val random = Random

  def write_qps(dest: FileWriter, qps: Int) = {
    val sleepTime = 1000 / qps
    while(true) {
      write(dest)
      Thread.sleep(sleepTime)
      messagesCount += 1
    }
  }

  def write(dest: FileWriter) = {
    val ip = ipgen.get_ip
    val request = pick_weighted_key(REQUESTS)

    val ext = pick_weighted_key(EXTENSIONS)
    val resp_code = pick_weighted_key(RESPONSE_CODES)
    val resp_size = random.nextInt(2 * 1024) + 192;
    val ua = pick_weighted_key(USER_AGENTS)
    val format = new java.text.SimpleDateFormat("dd/MMM/yyyy:HH:mm:ss Z")
    val date = format.format(new java.util.Date())
    try
    {
      val bw = new BufferedWriter(dest)
      bw.write(s"${ip} - - [${date}]" + " \"GET /" + s"${request} " + " HTTP/1.1\" " +
        s"${resp_code} ${resp_size} " + "\"-\" \"" + ua + "\"\n")
      bw.flush()
    }
    catch
      {
        case e: Exception => e.printStackTrace
      }
  }

  private def pick_weighted_key(map: Map[String, Int]): String = {
    var total = 0
    map.values.foreach { weight => total += weight }
    val rand = Random.nextInt(total)
    var running = 0
    for((key, weight) <- map) {
      if(rand >= running && rand < (running + weight)) {
        return key
      }
      running += weight
    }
    map.keys.head
  }
}

object RandomHttpLogGen extends App {

  val outputFile = new File(args(0))
  val messagesPerSec = args(1).toInt
  val outputFileWriter = new FileWriter(outputFile)

  sys.addShutdownHook {
    println("")
    println("Caught ^C, Aborting ...")
    println("closing file")
    outputFileWriter.close()
  }

  try {
    println(s"Generating random log events to ${outputFile} @ ${messagesPerSec}/second")
    new LogGenerator(new IPGenerator(100, 10)).write_qps(outputFileWriter, messagesPerSec)
  } catch {
    case e: Exception => e.printStackTrace
  }
  RandomHttpLogGen.main(args)
}

