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
    println("siu")
  }
}

