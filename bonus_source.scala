object project extends App {
  
  //Etape1
  
  type TupleD = (Int,String,String,String,Double,Double)
  def loadAirports(filename:String): Array[TupleD] = {
    //getting the file 
    val datalines = io.Source.fromFile(filename).getLines.toArray
    //val datafile = datalines.map(line => line.mkString)
    //now our file in an array of Strings lets make it a 2D array 
    val linesGifted = datalines.map(l => {
      //there ...
      val s = l.mkString
      val parts = s.replace(", ","; ").split(",")
      val partsGifted = parts.patch(4, Nil, 2).patch(6, Nil, 6).toList   
      
      /*Le match et les commandes " .to'Something' "  permetent aussi de savoir si 
      on a bien construit notre tableau lors de la compilation*/
      
      val t = partsGifted match {
        case List(a,b,c,d,e,f) => (a.toInt,b,c,d,e.toDouble,f.toDouble)
      }
      t
    })
    //now linesGisted est notre tableau de Tuples
    return linesGifted
  }
  /*à noter que l'adresse du fichier doit changer selon l'emplacement du fichier 
  dans ton PC */
  //Check if the code works : println(loadAirports("/home/hatim/Workspace/airports.dat")(4)._5)

  //Etape2  
  //Commençons par trier notre tableau contenant les noms des aéroports et les coordonnées
  
  def Tri(tab:Array[TupleD]): Array[TupleD] = {
    def quickSort(lst:List[TupleD]):List[TupleD] = lst match {
      case Nil => Nil
      case x::Nil => lst
      case _ =>
        val p = lst.head
        val (before,after) = lst.tail.partition(_._2<p._2)
        quickSort(before) ++ (p :: quickSort(after))
    }
    return quickSort(tab.toList).toArray
  }
  
  //Notre tableau trié : tabiS 
  
  
  //Les fonctions mathematiques pour calculer la distance
  def sin(x: Double): Double = java.lang.Math.sin(x)
  def cos(x: Double): Double = java.lang.Math.cos(x)
  def acos(x: Double): Double = java.lang.Math.acos(x)
  val R = 6371
  val pi = 180.toRadians
  
  //La fonction qui recherche l'indice d'un aeroport dans dans tableau
  def recherche(tab:Array[TupleD], elmi:String):Int = {
    val elmf = elmi.filterNot(_ == '"')
    def rechercheRec(tab:Array[TupleD], elm:String, min:Int, max:Int):Int = {
      if (max-min < 2) {
        if (tab(min)._2.filterNot(_ == '"') == elm) {
          return min
        } else {
          return max
        }
      } else {
        val mid = (min+max) / 2
        val cmid = tab(mid)._2.filterNot(_ == '"')
        // println(elm<cmid) //: help when searching for errors    
        if (elm < cmid) {
        //println(" Debut ("+min+";"+mid+")"+elm+"-"+cmid+"-") //: help when searching for errors
          return rechercheRec(tab,elm,min,mid)
        } else {
          //println(" Debut ("+mid+";"+max+")"+elm+"-"+cmid+"-") //: help when searching for errors
          return rechercheRec(tab,elm,mid,max)
        }
      }  
    }
    return rechercheRec(tab,elmf,0, tab.length-1)
  }
   
  
  //Fonction qui recherche les coordonees d'un aeroport
  def chercheCoord(airportx:String, tab:Array[TupleD]): Array[Double] = {
    val coord = new Array[Double](2)
    coord(0) = tab(recherche(tab,airportx))._5    //latitude 
    coord(1) = tab(recherche(tab,airportx))._6    //longtitude
    return coord 
  }        
  //Fonction qui calcule la distance entre 2 aeroports
  def Distance(airport1:String , airport2:String, tabi:Array[TupleD]): Double = {
    val coord1 = chercheCoord(airport1,tabi).map(c => c.toRadians)
    val coord2 = chercheCoord(airport2,tabi).map(c => c.toRadians)
    return R*acos((sin(coord1(0))*sin(coord2(0))) + (cos(coord1(0))*cos(coord2(0))*cos(coord1(1) - coord2(1)))) 
  }
  //Check if the code works : println(Distance("Ibn Batouta Airport","Barcelona International Airport",Tri(loadAirports("/home/hatim/Workspace/airports.dat"))))
 

  //La fonction : alld qui calcule toutes les distances 
  def alld(tabi:Array[TupleD]): Array[Array[Double]] ={
    val alldistance = Array.ofDim[Double]((tabi.length*(tabi.length-1)/2),3) 
    var index = 0
    // On a j>i c'est très important car on ne calcule pas la meme distance 2 fois
    for (i <- 0 to tabi.length-2){ 
      for (j <- i+1 to tabi.length-1){
        alldistance(index)(0) = i
        alldistance(index)(1) = j
        alldistance(index)(2) = R*acos((sin(tabi(i)._5.toRadians)*sin(tabi(j)._5.toRadians)) + (cos(tabi(i)._5.toRadians)*cos(tabi(j)._5.toRadians)*cos(tabi(i)._6.toRadians - tabi(j)._6.toRadians)))
        index = index + 1
      }
    }
    return alldistance  
  }
  
  // Check if the code works : println(alld(loadAirports("/home/hatim/Workspace/projetTOP/airports.dat"))(0)(2))
   
  //Etape 3 :
  
  //D'abord il faut trier notre tableau de distances
  def quickSort(lst:List[Array[Double]]):List[Array[Double]] = lst match {
      case Nil => Nil
      case x::Nil => lst
      case _ =>
        val p = lst.head
        val (before,after) = lst.tail.partition(_(2)<p(2))
        quickSort(before) ++ (p :: quickSort(after))
    }
  def Statistiques(tab:Array[TupleD]){
    def sqrt(x: Double): Double = java.lang.Math.sqrt(x)
    val tabD = quickSort(alld(tab).toList).toArray
    val l = tabD.length-1
    val median = (tabD(l/2 - 1)(2) + tabD(l/2)(2))/2
    val min = tabD(0)(2)
    val max = tabD(l-1)(2)
    var a = 0.0
    var sr = 0.0
    for (i <- tabD){
      a = a + i(2)
    }
    a = a/l
    for (i <- tabD){
      sr = sr + ((i(2) - a)*(i(2) - a))
    }
     
    println("Maximum is : " + max + "km" )
    println("Minimum is : " + min + "km")
    println("Median is : " + median + "km")
    println("Mean is : " + a + "km")
    println("Standard deviation is : " + sqrt(sr/l) + "km")
  }
  //Check if the code works : Statistiques(loadAirports("/home/hatim/Workspace/airports.dat"))
  
  
  //Etape 4

  def Statistiquesbis(tab:Array[Array[Double]]){
    def sqrt(x: Double): Double = java.lang.Math.sqrt(x)
    val tabD = quickSort(tab.toList).toArray
    val l = tab.length-1
    val median = (tab(l/2 - 1)(2) + tab(l/2)(2))/2
    val min = tab(0)(2)
    val max = tab(l-1)(2)
    var a = 0.0
    var sr = 0.0
    for (i <- tab){
      a = a + i(2)
    }
    a = a/l
    for (i <- tab){
      sr = sr + ((i(2) - a)*(i(2) - a))
    }
     
    println("Maximum is : " + max + "km" )
    println("Minimum is : " + min + "km")
    println("Median is : " + median + "km")
    println("Mean is : " + a + "km")
    println("Standard deviation is : " + sqrt(sr/l) + "km")
  }
  
  def filtrePays(listPays:List[String],tab:Array[TupleD]): Array[Array[Double]] = {
    var alldistance:Array[Array[Double]] = Array() 
    for (i <- 0 to tab.length-2){ 
      for (j <- i+1 to tab.length-1){
        var a = tab(i)
        var b = tab(j)
        if (listPays.contains(a._4.filterNot(_ == '"')) && listPays.contains(b._4.filterNot(_ == '"'))) {
          var A = a._5.toRadians
          var B = b._5.toRadians 
          alldistance = alldistance :+ (Array(i,j,R*acos((sin(A)*sin(B)) + (cos(A)*cos(B)*cos(a._6.toRadians - b._6.toRadians)))))
          //Check : println(a._2,b._2,R*acos((sin(A)*sin(B)) + (cos(A)*cos(B)*cos(a._6.toRadians - b._6.toRadians))))
        }
      }
    }
    return alldistance
  }
  //Check if the code works : filtrePays(List("France","Germany"),loadAirports("/home/hatim/Workspace/projetTOP/airports.dat"))
  
  def filtreHemisphere(hem:String,tab:Array[TupleD]): Array[Array[Double]] = {
    var alldistance:Array[Array[Double]] = Array() 
    for (i <- 0 to tab.length-2){ 
      for (j <- i+1 to tab.length-1){
        var a = tab(i)
        var b = tab(j)
        if (hem == "nord") {
          if ( a._5 > 0 && b._5 > 0) {
            var A = a._5.toRadians
            var B = b._5.toRadians 
            alldistance = alldistance :+ (Array(i,j,R*acos((sin(A)*sin(B)) + (cos(A)*cos(B)*cos(a._6.toRadians - b._6.toRadians)))))
            //Check  : println(a._2,b._2,R*acos((sin(A)*sin(B)) + (cos(A)*cos(B)*cos(a._6.toRadians - b._6.toRadians))))
          }
        }
        if (hem == "sud") {
          if ( a._5 < 0 && b._5 < 0) {
            var A = a._5.toRadians
            var B = b._5.toRadians 
            alldistance = alldistance :+ (Array(i,j,R*acos((sin(A)*sin(B)) + (cos(A)*cos(B)*cos(a._6.toRadians - b._6.toRadians)))))
          }
        }
        if (hem == "est") {
          if ( a._6 > 0 && b._6 > 0) {
            var A = a._5.toRadians
            var B = b._5.toRadians 
            alldistance = alldistance :+ (Array(i,j,R*acos((sin(A)*sin(B)) + (cos(A)*cos(B)*cos(a._6.toRadians - b._6.toRadians)))))
          }
        }
        if (hem == "ouest") {
          if ( a._6 < 0 && b._6 < 0) {
            var A = a._5.toRadians
            var B = b._5.toRadians 
            alldistance = alldistance :+ (Array(i,j,R*acos((sin(A)*sin(B)) + (cos(A)*cos(B)*cos(a._6.toRadians - b._6.toRadians)))))
          }
        }
      }
    }
    return alldistance
  }
  //Check : filtreHemisphere("nord",loadAirports("/home/hatim/Workspace/projetTOP/airports.dat"))
  
  
  def filtre2Pts(lata:Double,longa:Double,latb:Double,longb:Double,tab:Array[TupleD]): Array[Array[Double]] = {
    var alldistance:Array[Array[Double]] = Array() 
    for (i <- 0 to tab.length-2){ 
      for (j <- i+1 to tab.length-1){
        var a = tab(i)
        var b = tab(j)
        var minlat = scala.math.min(lata,latb).toRadians
        var maxlat = scala.math.max(lata,latb).toRadians
        var minlong = scala.math.min(longa,longb).toRadians
        var maxlong = scala.math.max(longa,longb).toRadians
        if (minlat < a._5.toRadians && a._5.toRadians < maxlat && minlong < a._6.toRadians && a._6.toRadians < maxlong ) {
          if (minlat < b._5.toRadians && b._5.toRadians < maxlat && minlong < b._6.toRadians && b._6.toRadians < maxlong){
            //if (minlat<b._5.toRadians && b._5.toRadians<maxlat && minlong<b._6.toRadians && b._6.toRadians<maxlong ){
            var A = a._5.toRadians
            var B = b._5.toRadians 
            alldistance = alldistance :+ (Array(i,j,R*acos((sin(A)*sin(B)) + (cos(A)*cos(B)*cos(a._6.toRadians - b._6.toRadians)))))
            //Check  : println(a._2,b._2,R*acos((sin(A)*sin(B)) + (cos(A)*cos(B)*cos(a._6.toRadians - b._6.toRadians))))  
          }
        }
      }
    }
    return alldistance
  }
  //Check : filtre2Pts(2,48,3,49,loadAirports("/home/hatim/Workspace/projetTOP/airports.dat"))) 
  
  def filtrePtRayon(lat:Double,long:Double,ray:Double,tab:Array[TupleD]): Array[Array[Double]] = {
    var alldistance:Array[Array[Double]] = Array() 
    for (i <- 0 to tab.length-2){ 
      for (j <- i+1 to tab.length-1){
        var a = tab(i)
        var b = tab(j)
        var A = a._5.toRadians
        var B = b._5.toRadians 
        var dist = R*acos((sin(A)*sin(B)) + (cos(A)*cos(B)*cos(a._6.toRadians - b._6.toRadians)))
        var dista = R*acos((sin(A)*sin(lat.toRadians)) + (cos(A)*cos(lat.toRadians)*cos(a._6.toRadians - long.toRadians)))
        var distb = R*acos((sin(lat.toRadians)*sin(B)) + (cos(lat.toRadians)*cos(B)*cos(long.toRadians - b._6.toRadians)))
        if ( dista < ray && distb < ray) {
          alldistance = alldistance :+ (Array(i,j,dist))
          //Check  : println((a._2,b._2,dist))))
        }
      }
    }
    return alldistance
  } 
  
  //Etape 5
  def Densite(populationDATA:String, surfaceDATA:String, tab:Array[TupleD]): Unit = {
    type Tuple4 = (String,Double)
    def loadextradata(filename:String): Array[Tuple4] = {
      //geting the file 
      val datalines = io.Source.fromFile(filename).getLines.toArray
      //val datafile = datalines.map(line => line.mkString)
      //now our file in an array of Strings lets make it a 2D array 
      val linesGifted = datalines.map(l => {
        //there ...
        val s = l.mkString.filterNot(_ == '"')
        val parts = s.replace(", ","; ").split(",")
        val partsGifted = parts.patch(1, Nil, 60).toList   
        /*Le match et les commandes " .to'Something' "  permetent aussi de savoir si 
        on a bien construit notre tableau lors de la compilation*/
      
        val t = partsGifted match {
          case List(a,b) => (a,b.toDouble)
        }
        t  
      })
      //now linesGisted est notre tableau de tuples
      return linesGifted
    }
    // Dans notre cas : tab = loadAirports("/home/hatim/Workspace/projetTOP/airports.dat")
    val surf = loadextradata(surfaceDATA)
    //val surf = loadextradata("/home/hatim/surface.txt")
    var exist = false
    var nbr = 0
    var index = 0
    // countryList va contenir : 
    var countryList = List(("",0.0,0.0,0))  //(Nom d'aeroport,population,surface,nbr d'aeroports)
    for (i <- loadextradata(populationDATA)){
      nbr = 0
      exist = false
      for (j <- tab){
        if (i._1 == j._4.filterNot(_ == '"')){
          exist = true
          nbr = nbr +1
        }
      }
      if (exist) {
        countryList = (i._1,nbr / i._2, nbr / surf(index)._2 ,nbr) :: countryList
        println("(" + i._1 + ",  " +  nbr/i._2 + " aeroports par personne ,  " + nbr/surf(index)._2 + " aeroports par km² ,  " + nbr  + " aeroports)")
      }
      index = index + 1
      // See if there is a airport in a country or not : println(i._1 + " exist : " + exist)
    }
  }
  //Check if the code works :  Densite("/home/hatim/population.txt","/home/hatim/surface.txt",loadAirports("/home/hatim/Workspace/projetTOP/airports.dat"))
  //Densite("/home/hatim/population.txt","/home/hatim/surface.txt",loadAirports("/home/hatim/Workspace/projetTOP/airports.dat"))

  //SUPP
  def distancebis(country:String, filename:String): Unit = {
    //Trions le tableau contenant les aeroports selon les  pays 
    def Tri(tab:Array[TupleD]): Array[TupleD] = {
      def quickSort(lst:List[TupleD]):List[TupleD] = lst match {
        case Nil => Nil
        case x::Nil => lst
        case _ =>
          val p = lst.head
          val (before,after) = lst.tail.partition(_._4<p._4)
          quickSort(before) ++ (p :: quickSort(after))
      }
      return quickSort(tab.toList).toArray
    }
    def Tri2(tab:List[String]): List[String] = {
      def quickSort(lst:List[String]):List[String] = lst match {
        case Nil => Nil
        case x::Nil => lst
        case _ =>
          val p = lst.head
          val (before,after) = lst.tail.partition(_<p)
          quickSort(before) ++ (p :: quickSort(after))
      }
      return quickSort(tab)
    }
    val tab = Tri(loadAirports(filename))
    var l = List("")
    var index = 0
    for (i <- tab){
      if ( i._4.filterNot(_ == '"') == country ){
        l = i._2 :: l
        index = index + 1
      }
    }
    println("There is : " + index + " airports" )
    for (i <- Tri2(l)){
      println(i)
    }
    
  }
  //Check iff the code works : distancebis("Japan","/home/hatim/Workspace/projetTOP/airports.dat")  

  def presentation(filename:String): Unit = {
    println("To see all the tuples extracted from" + filename + "  input : 1")
    println("To know the distance between 2 airports please input : 2")
    println("To see statistics about all distances betwen all the airports please input : 3")
    println("To see statistics based on filtred Data input : 4")
    println("To know the density of airports in each country by population and surface please input : 5")
    val a =scala.io.StdIn.readInt()
    a match {
      case 1 => {
        for(i <- loadAirports(filename)){
          println(i)
        }
        //memory info
        val mb = 1024*1024 
        val runtime = Runtime.getRuntime
        println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
        println("** Free Memory:  " + runtime.freeMemory / mb)
        println("** Total Memory: " + runtime.totalMemory / mb)
        println("** Max Memory:   " + runtime.maxMemory / mb)

      }
      case 2 => {
        println("If you want to type directly the names of the airports wich u want to calculate the distance write 'true' else you can search by country's name therefore write 'false' ?")
        val resp =scala.io.StdIn.readBoolean()
        if(resp){
          println("Write the name of the first airport")
          val airporta =scala.io.StdIn.readLine()
          println("Write the name of the first airport")
          val airportb =scala.io.StdIn.readLine()
          println(Distance(airporta,airportb,Tri(loadAirports(filename))) + " km")
          println("Are you are not satisfied with your answer?If not we can use the second option to find your airport")
          val re =scala.io.StdIn.readLine()
          if(re == "no"){
            println("To search for the first airport type a country's name :")
            val countrya =scala.io.StdIn.readLine()
            distancebis(countrya,filename)
            println("Type the airport you chose please :")
            val airporta =scala.io.StdIn.readLine()
            println("To search for the second airport type a country's name :")
            val countryb =scala.io.StdIn.readLine()
            distancebis(countryb,filename)
            println("Type the airport you chose please :")
            val airportb = scala.io.StdIn.readLine()
            println(Distance(airporta,airportb,Tri(loadAirports(filename))) + " km")
            //memory info
            val mb = 1024*1024 
            val runtime = Runtime.getRuntime
            println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
            println("** Free Memory:  " + runtime.freeMemory / mb)
            println("** Total Memory: " + runtime.totalMemory / mb)
            println("** Max Memory:   " + runtime.maxMemory / mb)
            println("Do you want to return to the main menu?")
            
            val re =scala.io.StdIn.readLine()
            if(re == "yes"){
              presentation(filename)
            }else{
              println("ok (but there is no options left)")
            }
          }else{
            //To return to the main menu
            println("Do you want to return to the main menu?")
            val re =scala.io.StdIn.readLine()
            if(re == "yes"){
              presentation(filename)
            }else{
              println("ok (but there is no options left)")
            }   
          }          
        }else{
          println("To search for the first airport type a country's name :")
          val countrya =scala.io.StdIn.readLine()
          distancebis(countrya,filename)
          println("Type the airport you chose please :")
          val airporta =scala.io.StdIn.readLine()
          println("To search for the second airport type a country's name :")
          val countryb =scala.io.StdIn.readLine()
          distancebis(countryb,filename)
          println("Type the airport you chose please :")
          val airportb = scala.io.StdIn.readLine()
          println(Distance(airporta,airportb,Tri(loadAirports(filename))) + " km")
          
          //memory info
          val mb = 1024*1024 
          val runtime = Runtime.getRuntime
          println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
          println("** Free Memory:  " + runtime.freeMemory / mb)
          println("** Total Memory: " + runtime.totalMemory / mb)
          println("** Max Memory:   " + runtime.maxMemory / mb)
          println("Do you want to return to the main menu?")
          val re =scala.io.StdIn.readLine()
          if(re == "yes"){
            presentation(filename)
          }
        }
      }  
      case 3 => {
        Statistiques(loadAirports(filename))

        //memory info
        val mb = 1024*1024 
        val runtime = Runtime.getRuntime
        println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
        println("** Free Memory:  " + runtime.freeMemory / mb)
        println("** Total Memory: " + runtime.totalMemory / mb)
        println("** Max Memory:   " + runtime.maxMemory / mb)
      }
      case 4 =>{
        println("Please choose a filter from the list below :")
        println(("filtre par Pays","filtre par 2 Points donnés","filtre par Hemisphere","filtre par un Point et un Rayon"))
        val filterc =scala.io.StdIn.readLine()
        filterc match {
          case "filtre par Pays" =>{
            println("Input a country : (to stop inputing type : Stop)")
            var l = List("")
            var country =scala.io.StdIn.readLine()
            while (country != "Stop"){
              l = country :: l
              country =scala.io.StdIn.readLine()
            }
            Statistiquesbis(filtrePays(l,loadAirports(filename)))
          }
          case "filtre par 2 Points donnees" =>{
            println("Input latitude of the first point :")
            val lat1 =scala.io.StdIn.readDouble()
            println("Input longitude of the first point :")
            val long1 =scala.io.StdIn.readDouble()
            println("Input latitude of the second point :")
            val lat2 =scala.io.StdIn.readDouble()
            println("Input longitude of the second point :")
            val long2 =scala.io.StdIn.readDouble()
            Statistiquesbis(filtre2Pts(lat1,long1,lat2,long2,loadAirports(filename)))

          }
          case "filtre par Hemisphere" =>{
            println("Select an area:   (nord,sud,est,ouest)")
            val are =scala.io.StdIn.readLine()
            Statistiquesbis(filtreHemisphere(are,loadAirports(filename)))

          }
          case "filtre par un Point et un Rayon" =>{
            println("Input latitude of the point :")
            val latt1 =scala.io.StdIn.readDouble()
            println("Input longitude of the point :")
            val longg1 =scala.io.StdIn.readDouble()
            println("Select a radius :")
            val radius =scala.io.StdIn.readDouble()
            Statistiquesbis(filtrePtRayon(latt1,longg1,radius,loadAirports(filename)))
          }
        }  
      }
      case 5 =>{
        Densite("/home/hatim/Workspace/population.txt","/home/hatim/Workspace/surface.txt",loadAirports(filename))
        //memory info
        val mb = 1024*1024 
        val runtime = Runtime.getRuntime
        println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
        println("** Free Memory:  " + runtime.freeMemory / mb)
        println("** Total Memory: " + runtime.totalMemory / mb)
        println("** Max Memory:   " + runtime.maxMemory / mb)
      }
    }
    println("Do you want to return to the main menu?")
    val re =scala.io.StdIn.readLine()
    if(re == "yes"){
      presentation(filename)
    }else{
      println("ok (but ther is no options left)")
    }
  }
  presentation("/home/hatim/Workspace/airports.dat")

}

