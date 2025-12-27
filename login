#!/usr/bin/env amm
import $ivy.`com.lihaoyi::os-lib:0.11.6`
import $ivy.`org.xerial:sqlite-jdbc:3.51.0.0`

import java.sql.{Connection, DriverManager, ResultSet}
import scala.util.{Try, Success, Failure}

def querySqlite[T](dbPath: os.Path, query: String)(processResults: ResultSet => T): T = {
  val conn = DriverManager.getConnection(s"jdbc:sqlite:$dbPath")
  try {
    val stmt = conn.createStatement()
    try {
      val rs = stmt.executeQuery(query)
      try {
        processResults(rs)
      } finally {
        rs.close()
      }
    } finally {
      stmt.close()
    }
  } finally {
    conn.close()
  }
}

def getFirefoxSession: Try[String] = {
  Try {
    // Find Firefox profile directory
    val firefoxDir = os.home / "snap" / "firefox" / "common" / ".mozilla" / "firefox"
    if (!os.exists(firefoxDir)) {
      throw new Exception(s"Firefox directory not found at $firefoxDir")
    }
    
    val profiles = os.list(firefoxDir).filter(x => os.isDir(x) && x.last.contains(".default-"))
    if (profiles.isEmpty) {
      throw new Exception(s"No Firefox profile found in $firefoxDir")
    }
    
    val cookiesDb = profiles.head / "cookies.sqlite"
    if (!os.exists(cookiesDb)) {
      throw new Exception(s"Cookies database not found at $cookiesDb")
    }
    
    // Copy database (Firefox locks it while running)
    val tmpDb = os.temp(cookiesDb.toSource, deleteOnExit = true, perms = "rw-------")
    
    // Query for adventofcode.com session cookie
    querySqlite(tmpDb, "SELECT value FROM moz_cookies WHERE host = '.adventofcode.com' AND name = 'session'") { rs =>
      if (rs.next()) {
        rs.getString("value")
      } else {
        throw new Exception("No session cookie found for adventofcode.com. Check that you're logged in!")
      }
    }
  }
}

@arg(doc = "Fetches the current Advent of Code session token and saves it to a special file.")
@main
def main(): Unit = {
  getFirefoxSession match {
    case Success(session) =>
      os.write.over(os.pwd / ".aoc-session", session, perms = "rw-------")
      println("[SUCCESS] Session token saved to .aoc-session")
    case Failure(e) =>
      println(s"[ERROR] ${e.getMessage}")
  }
}
