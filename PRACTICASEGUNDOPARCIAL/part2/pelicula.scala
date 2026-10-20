// EJERCICIO 1 
class Pelicula(
  val titulo: String,
  val duracionMinutos: Int,
  val clasificacion: String
)
object Pelicula{
    def apply(titulo: String,duracionMinutos: Int,clasificacion: String): Pelicula = {
        if duracionMinutos <= 0 then new Pelicula(titulo,90,clasificacion)
        else new Pelicula(titulo,duracionMinutos,clasificacion)
    }
}

// EJERCICIO 2
class FuncionCine(
  val pelicula: Pelicula,
  val sala: Int,
  val hora: String,
  val precioEntrada: Double,
  val asientosDisponibles: Int
){
    def puedeReservar(cantidad: Int): Boolean = {
        cantidad<=asientosDisponibles
    }

    def costoReserva(cantidad: Int): Double = {
        cantidad*precioEntrada
    }
    def reservar(cantidad: Int): FuncionCine = {
        FuncionCine(this.pelicula,this.sala,this.hora,this.precioEntrada,this.asientosDisponibles - cantidad)  
    }
    def descripcion: String = {
        s"\n${pelicula.titulo} \n" +
          s"Sala: $sala \n" +
          s"Hora: $hora \n" +
          s"Precio: $precioEntrada Bs \n" +
          s"Asientos disponibles: $asientosDisponibles"
    }
}
object FuncionCine{
    def apply(pelicula: Pelicula,sala: Int,
    hora: String,precioEntrada: Double,asientosDisponibles: Int): FuncionCine = {
        val salaf: Int => Int = salaAux => if salaAux>=1 then salaAux else 1
        val precioEntradaf: Double => Double = precioEntradaAux => if precioEntradaAux >= 0 then precioEntradaAux else 0
        val asientosDisponiblesf: Int => Int = asientosDisponiblesAux => if asientosDisponiblesAux >=0 then asientosDisponiblesAux else 0

        new FuncionCine(pelicula,salaf(sala),
        hora,precioEntradaf(precioEntrada),
        asientosDisponiblesf(asientosDisponibles))
    }

    def funcionEstandar(pelicula: Pelicula,sala: Int,hora: String): FuncionCine = {
         val salaf: Int => Int = salaAux => if salaAux>=1 then salaAux else 1
         new FuncionCine(pelicula,salaf(sala),hora,30,50)
    }
}




object pelicula_Main{
    def main(args: Array[String]): Unit = {
        val pelicula1 = Pelicula("Interstellar",169,"PG-13")
        println(pelicula1.titulo)
        val funcionCine1 = FuncionCine(pelicula1,-4,"15:00",35,30)
        println(funcionCine1.sala)
        println(funcionCine1.puedeReservar(30))
        println(funcionCine1.costoReserva(4)+" Bs")
        val funcionCine2 = funcionCine1.reservar(6)
        println(funcionCine2.asientosDisponibles)

        println(funcionCine1.descripcion)



        val pelicula2 = Pelicula("Dune",155,"PG-13")
        val funcion3 = FuncionCine(pelicula2,3,"20:30",35.0,40)
        val funcion4 = FuncionCine.funcionEstandar(pelicula2,1,"16:00") 
        println(s"\n${funcion3.descripcion}")
        println(s"${funcion4.descripcion}")
    }
}