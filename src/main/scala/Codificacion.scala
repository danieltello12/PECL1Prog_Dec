type Bit = 0 | 1

abstract class ArbolHuffman {
  def decodificar(nodo: ArbolHuffman, cadenaCodificada: List[Bit]): String = {

    def decodificarAux(nodo: ArbolHuffman, bits: List[Bit]): (String, List[Bit]) = {
      nodo match {
      case HojaHuffman (peso, caracter) => (caracter.toString, bits)
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

    /*
    No esta terminada, no funciona bien
     */

    def codificaraux(Nodo: ArbolHuffman, characteres: List[Char],bits: List[Bit]): List[Bit] = {
      Nodo match
        //En el caso que sea Hoja veo si el carácter coincide con el carácter de nuestra lista y devolvemos los bits en ese caso
        case HojaHuffman(peso, caracter) =>if caracter == characteres.head then bits else Nil
        //En el caso que sea Rama vamos primero por la izquierda. En el caso que fuésemos bien nos devolvería los bits del caso base
        //Si no resultadoIzquierda permanece vacío y habría que ir por la derecha

        case RamaHuffman(izquierda,derecha) =>

          val resultadoIzq=codificaraux(izquierda, characteres, bits:+1)
          if resultadoIzq.nonEmpty then resultadoIzq
            //Tenemos que ir por la derecha entonces
          else{

          val resultadoDer = codificaraux(derecha, characteres,bits:+0)
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
    //Si es una rama devuelvoel peso de la izquierda+ derecha
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

object Codificación {
  def main(args: Array[String]): Unit = {
    val arbol = RamaHuffman(HojaHuffman(4, 'S'), RamaHuffman(HojaHuffman(3, 'O'), RamaHuffman(HojaHuffman(2, 'S'), HojaHuffman(2, ' '))))
    val mensaje: List[Bit] = List(0,1,0,0,1,1,1,1,1,0,0,1,1,0,1,1,1,1,0,0,1,0)

    println("Peso del árbol: ${peso(arbol)}")
    println("Caracteres en el árbol: ${caracteres(arbol)}")
    println("Codificación en el árbol: " + arbol.codificar(arbol)("SOS "))
  }
}

