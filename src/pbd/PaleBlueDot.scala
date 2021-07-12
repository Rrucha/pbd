package pbd

import akka.util.ManifestInfo

import java.awt.Desktop
import java.net.URI
import scala.io.{BufferedSource, Source}

object PaleBlueDot {


  /**
   * Lecture Objective 1
   *
   * Converts degrees into radians
   *
   * @param degrees A value provided in degrees
   * @return The radian equivalent of the input value
   */
  def degreesToRadians(degrees: Double): Double = {
   degrees*Math.PI/180
  }



  /**
   * Lecture Objective 2
   *
   * Given a country name using and case (upper/lower), return the country code in all lowercase letters
   *
   * Ex. If "Heard Island and McDonald Islands#HM" is a line countriesFilename
   * and countryName is "hEaRd IsLaNd AnD mCdOnAlD iSlAnDs" the returned value is "hm"
   *
   * If countryName is not in the file, return the empty String: ""
   *
   * @param countriesFilename Name of the file containing country names and codes
   * @param countryName       The name of the country to lookup in the file with any mix of upper/lower-case
   * @return The two letter country code for countryName
   */
  def getCountryCode(countriesFilename: String, countryName: String): String = {
    var result : String = ""

    /** reading txt file and spliting */
    val nameOfCountriesFile: BufferedSource = Source.fromFile(countriesFilename)
    for (line <- nameOfCountriesFile.getLines()){
      val country_lower : String = line.toLowerCase()
      val country : Array[String] = country_lower.split("#" )

      /** finding the country */
      if (country(0) == countryName.toLowerCase){
         result =  result + country(1)
       }
      else{
      ""
      }
    }
      result
  }



  /**
   * Lecture Objective 3
   *val filename  = "data/countries.txt"
      val contents = getCountryCode(filename)
   * Returns a Map[cityName -> population] for all cities in the given county and region. The name of each
   * city should match exactly how it appears in citiesFilename and the population is read from the file
   * and converted to an Int.
   *
   * Ex: PaleBlueDot.cityPopulations(citiesFilename, "ad", "04") returns Map("la massana" -> 7211) since
   * "la massana" is the only city in region "04" of Andorra (code "ad") and its population is 7211
   *
   * @param citiesFilename Name of the file containing city name, population, and location data
   * @param countryCode    A two character country code
   * @param region         A two character region code
   * @return A Map containing the name and population of every city matching both the countryCode and region
   */

  def cityPopulations(citiesFilename: String, countryCode: String, region: String): Map[String, Int] = {
    var populationMap: Map[String, Int] = Map()

    /** reading txt file */
    val citiesFile: BufferedSource = Source.fromFile(citiesFilename)
     for (line <- citiesFile.getLines()) {
       val cities: Array[String] = line.split(",")

       /** finding the country population */
       if (cities(0).contains( countryCode) && cities(2).contains(region)) {
         populationMap = populationMap + (cities(1).toString -> cities(3).toInt)
       }
       else {
         ""
       }
     }
     populationMap
  }


  /**
   * Lecture Objective 4
   *
   * Returns a List of city names in the given county and region with a population at least minPopulation.
   *
   * @param citiesFilename Name of the file containing city name, population, and location data
   * @param countryCode    A two character country code
   * @param region         A two character region code
   * @param minPopulation  the minimum population that could be returned
   * @return All city names in countryCode/region with a population >= minPopulation
   */
  def bigCities(citiesFilename: String, countryCode: String, region: String, minPopulation: Int): List[String] = {
   var populationGreater: List[String] = List()

    /** reading txt file */
    val citiesFile: BufferedSource = Source.fromFile(citiesFilename)
    for (line <- citiesFile.getLines()) {
      val small_line : String = line.toLowerCase()
      val cities: Array[String] = small_line.split(",")

      /** finding the the country population */
      if ( cities(0).toLowerCase.contains(countryCode.toLowerCase()) && cities(2).toLowerCase.contains(region.toLowerCase()) ) {

        /** setting minimum value for population */
         if (cities(3).toInt >= minPopulation) {

          populationGreater =   cities(1) ::   populationGreater
         }
         else {
           ""
         }
      } else {
        ""
      }

    }
    populationGreater.sorted
  }


  /**
   * Lecture Objective 5
   *
   * Computes the greater circle distance ("As the crow flies") between two locations on Earth in kilometers.
   * The input locations are given as Lists of Double containing the latitude and longitude coordinates of that
   * location. For example, if the location is latitude: 35.685 and longitude: 139.751389 the input would be
   * List(35.685, 139.751389).
   *
   * @param location1 A location on Earth given as a List containing latitude and longitude coordinates
   * @param location2 A location on Earth given as a List containing latitude and longitude coordinates
   * @return The greater circle distance between the two input locations
   */
  def greaterCircleDistance(location1: List[Double], location2: List[Double]): Double = {
    val earthRadius: Double = 6371.0 // km

    /** converting */
    val lat1: Double = degreesToRadians(location1(0))
    val lon1: Double = degreesToRadians(location1(1))
    val lat2: Double = degreesToRadians(location2(0))
    val lon2: Double = degreesToRadians(location2(1))

    /** diff. latitude and longitude */
    val diffLat: Double = (lat2-lat1)
    val diffLon: Double = (lon2-lon1)

    /**  using formula */
    val x: Double =  Math.sin(diffLat/2) * Math.sin(diffLat/2) + Math.cos(lat1) * Math.cos(lat2) * Math.sin(diffLon/2) * Math.sin(diffLon/2);
    val y: Double = 2 * Math.atan2(Math.sqrt(x), Math.sqrt(1-x))


    val d: Double = (earthRadius * y )
    d

  }


  /**
   * Programming Objective 1
   *
   * You find yourself stranded in an unfamiliar place with no signs of civilization. You don't have much with you,
   * but you do have a locator that gives your current latitude/longitude, a csv file of cities, and your final
   * submission to the PaleBlueDot assignment from CSE116 (What luck!). You decide that finding and walking
   * directly to the closest city will give you the best chance to survive.
   *
   * @param citiesFilename Name of the file containing city name, population, and location data
   * @param location       A location on Earth given as a List containing latitude and longitude coordinates
   * @return The city closest to the given location as a List containing country code, city name, and region
   *         exactly as they appear in the cities file
   */
  def closestCity(citiesFilename: String, location: List[Double]): List[String] = {

    List()

  }


  /**
   * Programming Objective 2
   *
   * Find the population of a country by name. Not quite a life or death situation, but interesting information
   * regardless.
   *
   * @param countriesFilename Name of the file containing country names and codes
   * @param citiesFilename    Name of the file containing city name, population, and location data
   * @param countryName       The name of the country with any mix of upper/lower-case
   * @return The total population of the given country
   */
  def countryPopulation(countriesFilename: String, citiesFilename: String, countryName: String): Int = {
    var total_pop: Int = 0
    val name: String = countryName.toLowerCase()
    val code: String = getCountryCode(countriesFilename, name)
    /** reading txt file and spliting */
    val citiesFile: BufferedSource = Source.fromFile(citiesFilename)
    for (line <- citiesFile.getLines()) {
      val cities: Array[String] = line.split(",")
      /** checking country */
      if (cities(0).contains(code)) {
        total_pop = total_pop + cities(3).toInt

      }

      }
total_pop
  }


  /**
   * Application Objective
   *
   * You're in a city. I'm in a city. We want to meet in a city with a fair split of travel distance for each of us.
   * We happen to both own helicopters so we'll travel "as the crow flies" and we're not concerned about roads or
   * oceans. We just need to find the city closest to the midpoint between our two cities and we'll meet there.
   *
   * Each city is provided to this method as a List containing the country code, name, and region exactly as they
   * appear in the cities file (ie. Don't do anything with upper/lower-case in this method.) The returned city should
   * follow the same formatting (Don't modify the upper/lower-case of any Strings).
   *
   * @param citiesFilenam Name of the file containing city name, population, and location data
   * @param city1          A city as a List containing country code, name, and region exactly as they appear in the
   *                       cities file
   *                   every other city's distance from city1
   * @param city2          A city as a List containing country code, name, and region exactly as they appear in the
   *                       cities file
   * @return The city closest to the midpoint of the two input cities as a List containing country code, city name,
   *         and region exactly as they appear in the cities file
   */

  def latLonOflocation(citiesFilename: String, city: List[String]): List[Double] = {
    var answer : List[Double] = List()

    /** assigning values */
    val countryCode: String = city(0)
    val citycode: String = city(1)
    val region: String = city(2)

    /** reading txt file ans spliting */
    val citiesFile: BufferedSource = Source.fromFile(citiesFilename)
    for (line <- citiesFile.getLines()) {
      val cities: Array[String] = line.split(",")

      /** checking country and specifics */
      if (cities(0).contains(countryCode) && cities(2).contains(region) && cities(1).contains(citycode)) {
       val lat: Double = cities(4).toDouble
        val lon: Double = cities(5).toDouble
       answer =  answer :+ lat :+ lon
      }
    }
    answer
  }


  def midpoint(citiesFilename: String, city1: List[String], city2: List[String]): List[Double]={

    /** points of city1 **/
    val lon_lat1: List[Double] = latLonOflocation(citiesFilename, city1)
    val lat1: Double = degreesToRadians(lon_lat1(0))
    val lon1: Double = degreesToRadians(lon_lat1(1))

    /** points of city2 **/
    val lon_lat2: List[Double] = latLonOflocation(citiesFilename, city2)
    val lat2: Double = degreesToRadians(lon_lat2(0))
    val lon2: Double = degreesToRadians(lon_lat2(1))

    /** using mid point formula **/
    val x: Double = math.cos(lat2) * math.cos(lon2-lon1)
    val y: Double = math.cos(lat2) * math.sin(lon2-lon1)
    val midLat: Double = math.atan2(Math.sin(lat1) + math.sin(lat2), math.sqrt( (math.cos(lat1)+x)*(math.cos(lat1)+x) + y*y ) )
    val midLon: Double = lon1 + math.atan2(y, math.cos(lat1) + x)


    List(midLat*180/Math.PI,midLon*180/Math.PI)

  }


  def whereToMeet(citiesFilename: String, city1: List[String], city2: List[String]): List[String] = {
    var mylist2:List[String] = List()

     val mid: List[Double] = midpoint(citiesFilename, city1,city2)
    
    /** reading csv file **/
    val citiesFile: BufferedSource = Source.fromFile(citiesFilename)
    var small_Distance: Double = 987654321
    for (line <- citiesFile.getLines().drop(1)){
      val split: Array[String] = line.split(",")

      /** all points on cities.csv **/
      val genlat: Double = split(4).toDouble
      val genlon: Double = split(5).toDouble

      /** distance from midpoint to general points in file**/
      val Distance : Double = greaterCircleDistance(List(genlat,genlon), mid )
      if ( small_Distance > Distance ){

        /**  smallest distance will get replaced **/
        small_Distance = Distance

        /** list of small distances with "Country Code", "City Name" and "Region" **/
    mylist2 = mylist2 :+ split(0) :+ split(1) :+ split(2) :+ small_Distance.toString
      }
    }

    /** indexing of List */
    val info: Int = mylist2.indexOf(small_Distance.toString)
    val countrycode: String = mylist2(info-3)
    val cityname: String = mylist2(info-2)
    val regionname: String = mylist2(info-1)

    List(countrycode,cityname,regionname)

    }




  /**
   * Helper Method
   *
   * Opens Google Maps at a specific location. The location is a List containing the latitude then longitude as Doubles
   *
   * @param location The location to open in the format List(Latitude, Longitude)
   */
  def openMap(location: List[Double]): Unit = {
    if (Desktop.isDesktopSupported && Desktop.getDesktop.isSupported(Desktop.Action.BROWSE)) {
      val url: String = "http://maps.google.com/maps?t=m&q=loc:" + location.head.toString + "+" + location(1).toString
      Desktop.getDesktop.browse(new URI(url))
    } else {
      println("Opening the browser not supported")
    }
  }


  def main(args: Array[String]): Unit = {

    openMap(List(43.002743, -78.7874136))
       println(getCountryCode("data/countries.txt","iNdia"))
     println(cityPopulations("data/cities.csv","zw","06"))
      println(bigCities("data/cities.csv","US","", 100000))
      println(greaterCircleDistance(List(30,20),List(10,11)))
      println(countryPopulation("data/countries.txt","data/cities.csv", "iNdia"))

       println(greaterCircleDistance(List(21,20),List(10,11)))
      println(whereToMeet("data/cities.csv", List("us","buffalo","NY"), List("us","buffalo","NY")))
       println(whereToMeet("data/cities.csv", List("in","delhi","07"), List("in","gandhinagar","09")))
      println(greaterCircleDistance(List(20,20),List(10,-10)))


}}
