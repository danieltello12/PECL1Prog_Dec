type Bit = 0 | 1

abstract class ArbolHuffman {

  def decodificar(nodo: ArbolHuffman, cadenaCodificada: List[Bit]): String = {

    def decodificarAux(nodo: ArbolHuffman, bits: List[Bit]): (String, List[Bit]) = {
      nodo match {
      case HojaHuffman (_, caracter) => (caracter.toString, bits)
          /*
          En caso de que el nodo introducido sea de tipo Hoja
          devolvere el caracter en formato string y los bits
           */
      case RamaHuffman (izquierda, derecha) => bits match {
        //En caso de que el nodo sea rama, comparo los bits
        case 1 :: cola =>
          /*
            Si el Bit que toca es uno tomare un valor como tupla en el que guardare
          el resultado de la decodificacion de la derecha y lo bits que quedan
           */
          val (decodif_derecha, resto) = decodificarAux(derecha, cola)
          (decodif_derecha, resto)
        case 0 :: cola =>
          /*
          Si el bit que toca es 0 tomare un valor como tupla en el que guardare
          el resultado de la decodificacion de la izquierda y los bits que quedan
           */
          val (decodif_izquierda, resto) = decodificarAux(izquierda, cola)
          (decodif_izquierda, resto)

        case Nil => ("", bits)
        //Si la lista de bits es vacia devuelvo un caracter vacio
      }
      }
      }


    val resultado = new StringBuilder //Inicializo valor para el resultado
    var resto = cadenaCodificada  // Inicializo una variable resto que ira conteniendo lo que vaya quedando de bits

    /*
   Hago un bucle while para ir recorriendo la lista de bits hasta que este vacia
    e ir guardando los caracteres en un String Builder
     */

    while (resto.nonEmpty) {
      val (caracterDecodificado, restoActual) = decodificarAux(nodo, resto)
      resultado.append(caracterDecodificado)
      resto = restoActual
    }
    resultado.toString() //Paso el String Builder a formato String
  }

  def codificar(arbol: ArbolHuffman)(cadena: String): String =
    def codificaraux(Nodo: ArbolHuffman, characteres: List[Char],bits: List[Bit]): List[Bit] = {
      Nodo match
        //En el caso que sea Hoja veo si el carácter coincide con el carácter de nuestra lista y devolvemos los bits en ese caso
        case HojaHuffman(peso, caracter) =>if caracter == characteres.head then bits else Nil
        //En el caso que sea Rama vamos primero por la izquierda. En el caso que fuésemos bien nos devolvería los bits del caso base
        //Si no resultadoIzquierda permanece vacío y habría que ir por la derecha

        case RamaHuffman(izquierda,derecha) =>

          val resultadoIzq=codificaraux(izquierda, characteres, bits:+0)
          if resultadoIzq.nonEmpty then resultadoIzq
            //Tenemos que ir por la derecha entonces
          else{

          val resultadoDer = codificaraux(derecha, characteres,bits:+1)
          resultadoDer
          }



    }
    //Pasamos la cadena a una lista
    var caracteres = cadena.toList
    //Definimos una lista para ir metiendo los bits correspondientes a la cadena
    var bitList: List[Bit] = List()
    //Mientras que la cadena metida no sea vacía vamos llamando al auxiliar y quedándonos con la cola de caracteres
    //De manera que en el caso base del auxiliar vemos si nos coincide el carácter que hay en esa determinada posición
    //De la cadena con la HojaHuffman
    while (caracteres.nonEmpty) {
      var bits = codificaraux(arbol, caracteres, List())
      caracteres=caracteres.tail
      //Vamos almacenando los bits
      bitList=bitList:::bits

    }
    bitList.toString()

}

//CLASE CASO RAMA HUFFMAN QUE HEREDA DE ARBOLHUFFMAN

case class RamaHuffman(izquierda: ArbolHuffman, derecha: ArbolHuffman) extends ArbolHuffman

//CLASE CASO HOJA HUFFMAN QUE HERED DE ARBOLHUFFMAN

case class HojaHuffman(peso: Int, caracter: Char) extends ArbolHuffman

def peso(arbol: ArbolHuffman): Int = arbol match {
  //Comparo el tipo de arbol que es
  case HojaHuffman(p, _) =>
  //Si es una hoja devuelvo el peso
  p

  case RamaHuffman(izq, dch) =>
    //Si es una rama devuelvo el peso de la izquierda+ derecha
    peso(izq) + peso(dch)
}

def caracteres(arbol: ArbolHuffman): List[Char] = arbol match {
  //Comparo el tipo de arbol que es
  case HojaHuffman(_, c) =>
    //Si es una hoja devuelvo una lista con el caracter
    List(c)
  case RamaHuffman(izq, dch) =>
    //Si es una rama concateno en lista los caracteres de la izquierda y los de la derecha
    caracteres(izq) ++ caracteres(dch)
}
def ListaCharsADistFrec(listaChar: List[Char]): List[(Char, Int)] ={
  /*
  Creo una función auxiliar que mediante la función count
  me va a contar en la lista caracteres cuantas veces aparece el caracter que le indique
   */
  def auxListChar(caracteres: List[Char], caracter:Char):Int={
    caracteres.count(_==caracter)
  }

  val caracteres= listaChar.distinct //Creo una variable para almacenar todos los caracteres distintos que hay
  val listatuplas: List[(Char,Int)]=caracteres.map(char=>(char,auxListChar(listaChar,char)))
  /*Hago  la lista de tuplas utilizando el mapeado pasando cada caracter a el caracter y al lado
   su frecuencia mediante la funcion auxiliar
   */
  listatuplas //Devuelvo listatuplas
}
// Convierte la distribución en una lista de hojas ordenada
def DistribFrecAListaHojas(frecuencias: List[(Char,Int)]): List[HojaHuffman] ={
  /*
      Creo una función auxiliar para recorrer la lista de tuplas y segundo la recorro
      creo la hoja y la añado a la lista de hojas
   */
  def auxHojas(frecuencias: List[(Char,Int)], hojas: List[HojaHuffman]):List[HojaHuffman]={
    frecuencias match
      case Nil=> hojas // Si la lista de frecuencias es vacia devuelvo ya las hojas
      case cabeza::cola=> val nuevaHoja: HojaHuffman=HojaHuffman(cabeza._2,cabeza._1)
                            auxHojas(cola, hojas.appended(nuevaHoja))
                            /*
                            En caso de que tenga cabeza y cola creo una hoja con los valores correspondientes
                            de la tupla y llamo de nuevo a la funcion auxiliar conla cola y la lista de hojas con
                            la nueva hoja
                             */
  }
  auxHojas(frecuencias.sortBy(_._2),List())
  /*
  Llamo a la función auxiliar con la lista de tuplas  ordenadas por el segundo parametro,
  es decir por la frecuencia
   */
}
// Crea un objeto RamaHuff integrando los dos ArbolHuff (izquierdo y
// derecho)que se le pasan como parámetros
def creaRamaHuff(izq: ArbolHuffman, dch: ArbolHuffman):RamaHuffman =
  val rama:RamaHuffman= RamaHuffman(izq,dch)
  rama

def combinar(nodos: List[ArbolHuffman]): List[ArbolHuffman] = {
  def combinarAux(nodos: List[ArbolHuffman]): List[ArbolHuffman] = nodos match {
    //Si solo tiene un elemento me quedo con él
    case List(nodo) => List(nodo)
    case cabeza :: cola =>
      val (primerosDos, resto) = nodos.splitAt(2)

      val List(izq, dch) = primerosDos // Extraemos los dos primeros nodos

      //Utilizamos función hecha anteriormente
      val nuevaRama = creaRamaHuff(izq, dch)


      val nuevaLista = (nuevaRama :: resto)
      val pesoRama = peso(nuevaRama)
      //Aplico inserción para comparar los pesos de la rama general con el peso de la hoja.
      def insertar(nuevaRama: ArbolHuffman, ordenada: List[ArbolHuffman]): List[ArbolHuffman] = ordenada match {

        case Nil => List(nuevaRama)
        case h :: t => if (peso(nuevaRama) <= peso(h)) nuevaRama :: ordenada else h :: insertar(nuevaRama, t)
      }

      def ordenar(noOrdenada: List[ArbolHuffman], ordenada: List[ArbolHuffman]): List[ArbolHuffman] = noOrdenada match {
        case Nil => ordenada
        case h :: t => ordenar(t, insertar(h, ordenada))


      }

      ordenar(nuevaLista, Nil)


  }

  combinarAux(nodos)

}

  def esListaSingleton(lista: List[ArbolHuffman]): Boolean = lista match{
    case List(_)=>true //Tenga un solo elemento
    case _=>false //En otro caso
  }

def repetirHasta(combinar: List[ArbolHuffman] => List[ArbolHuffman], esListaSingleton: List[ArbolHuffman] => Boolean)(listaHojas: List[ArbolHuffman]): List[ArbolHuffman] =
  def auxiliar(nodos: List[ArbolHuffman]):List[ArbolHuffman]=
      if esListaSingleton(nodos) then nodos //Solo tiene un elemento por lo que devuelvo nodos
        //Si no llamo a auxiliar combinando los nodos quedándote cada vez con menos y finalmente el booleano se pondrá a true
        //Y devolverá la lista con todos los elementos del árbol
      else auxiliar(combinar(nodos))

  auxiliar(listaHojas)
def crearArbolHuffman(cadena: String): ArbolHuffman =
  //Aplico todas las funciones
  val tupla=ListaCharsADistFrec(cadena.toList)
  val ordenada=DistribFrecAListaHojas(tupla)
  val resultado=repetirHasta(combinar,esListaSingleton)(ordenada)
  //Como se reduce a un solo nodo combinando devolvemos la cabeza
  resultado.head
object  ArbolHuffman {
  def apply(cadena: String): ArbolHuffman = crearArbolHuffman(cadena)

}
type TablaCodigos = List[(Char, List[Bit])] //Creo el tipo de Dato TablaCodigos
def deArbolATabla(arbol: ArbolHuffman): TablaCodigos = {
  /*
    Utiliza una función auxiliar para recorrer el arbol
   */
  def auxArbolTabla(Nodo: ArbolHuffman, bits: List[Bit], hojaHuffman: HojaHuffman): (Char, List[Bit]) = {
    Nodo match //Comparo el Nodo en el que me encuentro
      case HojaHuffman(_, caracter) => { //Si estoyen una hoja guardo el caracter
        if hojaHuffman.caracter == caracter then (caracter, bits)
          /*
          Si la hoja en la que me encuentro es la hoja que estoy buscando (las comparo por los caracteres)
          devuelvo una tupla con el caracter de la hoja y la lista de bits del camino hasta la hoja
           */
        else (caracter, Nil) //Si no es la hoja q busco devuelvo Nil en la lista de bits del camino hacia la hoja
      }
      case RamaHuffman(izquierda, derecha) => //Caso rama con izquierda y derecah
        val resultadoIzq = auxArbolTabla(izquierda, bits :+ 0, hojaHuffman)
        /*
          Creo una valor que va a ser el resultado de la izquierda que se metera po la rama de la izquierda
        hasta llegar a una hoja

        A la cadena de bits le añado un 0 para indicar que se ha metido por la izquierda
         */
        if resultadoIzq._2.nonEmpty then resultadoIzq
          /*
          Si el valor 2 de la tupla no esta vacio, es decir ha llegado a la hoja y era esa hoja la que estabaos buscando
          devuevo la tupla
           */
        else //Si por la izquierda esta vacia quiere decir que a la izquierda no esta la hoja, luego vamos a la derecha
          val resultadoDer = auxArbolTabla(derecha, bits :+ 1, hojaHuffman)
          /*
          Creo una valor que va a ser el resultado de la derecha que se metera por la rama de la derecha
                  hasta llegar a una hoja

          A la cadena de bits le añado un 1 para indicar que se ha metido por la derecha
           */

          resultadoDer //Devuelvo el resultado de la derecha


  }

  var tabla: TablaCodigos = List() //Creo una variable tabla para ir añadiendo los resultados

  //Creo una variable con una lista de las hojas que hay dentro del arbol
  var listaHojas: List[HojaHuffman] = DistribFrecAListaHojas(ListaCharsADistFrec(caracteres(arbol)))
  while (listaHojas.nonEmpty) { //Hago un while hasta que no queden más hojas por buscar
    val (caracter, codigo) = auxArbolTabla(arbol, List(), listaHojas.head)
    tabla = tabla.appended(caracter, codigo) //Añado a la tabla la tupla resultante
    listaHojas = listaHojas.tail //Quito de la lista de hojas la hoja que he encontrado
  }
  tabla //Devuelvo la tabla
}
def codificar(arbol: TablaCodigos)(cadena: String): List[Bit]={
  def codificaraux(arbol: TablaCodigos,caracteres: List[Char]):List[Bit]={ //Creo función auxiliar para recorrer la tabla
    caracteres match //Comparo los caracteres
      case Nil => Nil

      case cabeza::cola=>//En el caso de que tenga cabeza y cola los caracteres
        if(arbol.head._1==cabeza) then arbol.head._2
          /*
          Si el primer elemento de la tabla tiene el mismo caracter que el primer elemento de la cadena
          devuelvo el codigo de dicho caracter
           */
        else codificaraux(arbol.tail,caracteres)
        /*
        En caso contrario llamo de nuevo a la función auxiliar con el resto de elementos de la tabla
        y los caracteres
         */
  }
  var caracteres= cadena.toList //Creo la cadena de caracteres
  var codigo: List[Bit]= List() //Creo una lista vacia donde guardar el codigo final
  while (caracteres.nonEmpty){
    codigo=codigo++codificaraux(arbol,caracteres) //Añado a la lista del codigo el codigo que me devuelve la funcion
    caracteres=caracteres.tail //Quito el primer elemento de los caracteres
  }
codigo //Devuelvo el código
}

def decodificar(tabla: TablaCodigos)(cadena: List[Bit]): String={
  //Función auxiliar para recorrer la tabla en busca de el caracter que coincida con el codiog que se recibe
  def auxiliarDecod(tabla: TablaCodigos, cadena: List[Bit]): Option[Char]={
    tabla match
      case Nil => None
      case cabeza::cola => //Si la tabla tiene cabeza y cola
        /*
        Si el primer elemento de la tabla tiene el mismo código que el código de bits
        que se ha pasado por parametro devuelvo el primer valor de la primera tupla, es decir el caracter
         */
        if(cabeza._2.equals(cadena)) then Some(cabeza._1)
          /*
          Si no llamo de nuevo a la función auxiliar con el resto de elemento de la tabla y con la cadena de bits
           */
        else auxiliarDecod(cola,cadena)
  }
  val resultado= new StringBuilder() //Creo un valor stringBuilder para guardar el resultado
  var bits: List[Bit]= List() //Creo una variable para guardar las cadenas de bits que voy a ir buscando
  bits=bits.appended(cadena.head) //Añado a la lista vacia el primer bit de la cadena
  var copia=cadena.tail //Creo un valor con una copia de la cadena para ir modificandola segun necesite
  while (copia.nonEmpty|| bits.nonEmpty) { //Mientras que la copia y la lista de bits no este vacia
    val caracter = auxiliarDecod(tabla,bits)
    /*
    Si el caracter que me devuelve la función es None, significa que no ha encontrado ningún caracter
    en la tabla con ese codigo de bits luego voy a añadir el siguiente bit de la copia en la cadena de bits
    que estoy utiliando para buscar los caracteres
     */
    if(caracter==None) then {
      if(copia.nonEmpty) {
        bits = bits.appended(copia.head)
        copia = copia.tail
      }
      else{
        bits= bits.empty
      }
    }
    else{
      caracter match
        /*
        Como la funcion me devuelve Optiona[Char] utilizo el match para meter en el stringBuilder
        solo el caracter y no Some(Char)
         */
        case Some(x) => resultado.append(x)
      if(copia.nonEmpty) then {
        /*
        Borro los bits de la lista de bits para buscar ya que he encontrado ya el caracter y pongo
        en su lugar el siguiente bit de la cadena
         */
        bits = List(copia.head)
        copia = copia.tail
        //Dejo en la cadena todo menos el bit que acabo de meter en la lista de bits para buscar
      }else{
        bits=bits.empty
        /*
        Si la copia esta vacia vacio la lista de bits para buscar para poder salir del bucel while

         */
      }
    }
  }
  resultado.toString()

}



object Codificación {
  def main(args: Array[String]): Unit = {
    val arbol = RamaHuffman(HojaHuffman(4, 'S'), RamaHuffman(HojaHuffman(3, 'O'), RamaHuffman(HojaHuffman(2, 'E'), HojaHuffman(2, ' '))))
    val mensaje: List[Bit] = List(0,1,0,0,1,1,1,1,1,0,0,1,1,0,1,1,1,1,0,0,1,0)
    val prueba: String= "holaaa buenss"

    println(s"Peso del árbol: ${peso(arbol)}")
    println(s"Caracteres en el árbol: ${caracteres(arbol)}")
    println(s"Decodificación en arbol: ${arbol.decodificar(arbol,mensaje)}")
    println("Codificación en el árbol: " + arbol.codificar(arbol)("SOS ESE OSO"))
    println("Frecuencias: "+ListaCharsADistFrec(prueba.toList).sortBy(_._2).toString())
    println("Hojas: "+DistribFrecAListaHojas(ListaCharsADistFrec(prueba.toList)).toString())
    val arbol2=crearArbolHuffman("jeierrw")
    val tabla= deArbolATabla(arbol2)
    val tabla2= deArbolATabla(arbol);
    println(codificar(tabla2)("SOS ESE OSO").toString())
    println(arbol.codificar(arbol)("SOS ESE OSO"))
    println(decodificar(tabla2)(codificar(tabla2)("SOS ESE OSO")))
    println("siu")
  }
}

