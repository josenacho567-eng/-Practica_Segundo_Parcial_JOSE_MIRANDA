object ejerciciosPart1{

    def transformar(numero: Int, operacion: Int => Int ): Int = {
        operacion(numero)
    }

    def crearMultiplicador(factor: Int): Int => Int = {
        (num)=> factor *num
    }

    def calcular(a: Double,b: Double,operacion: (Double, Double) => Double): Double ={
        operacion(a,b)
    }

    def aplicarDescuento(precio: Double,porcentaje: Double): Double = {
        precio - (precio*porcentaje)
    }

    def elevar(exponente: Int)(base: Int): Int = {
        if exponente == 0 then 1
        else base * elevar(exponente -1)( base)
    }

    def sumar(desde: Int,hasta: Int,transformar: Int => Int): Int = {
        if desde == hasta then transformar(hasta)
        else transformar(desde) + sumar(desde+1,hasta,transformar)
    }

    def producto(desde: Int,hasta: Int,transformar: Int => Int): Int = {
        if desde == hasta then transformar(hasta)
        else transformar(desde) * producto(desde+1,hasta,transformar)
    }


    
    def transformarTexto(texto: String,transformacion: Char => Char): String = {
        if texto.length() == 1 then s"${transformacion(texto.head)}"
        else s"${transformacion(texto.head)}${transformarTexto(texto.tail,transformacion)}"
    }

    def buscarCaracter(texto: String,condicion: Char => Boolean): Boolean = {
        if texto.length() == 1 then condicion(texto.head)
        else if condicion(texto.head) then true 
        else buscarCaracter(texto.tail,condicion)
    }

    def main(args: Array[String]): Unit = {
        // Ejercicio 1
        val duplicar: Int => Int = num => num*2
        val cuadrado: Int => Int = num => num*num
        val esPar: Int => Boolean = num => num%2 == 0

        println(duplicar(6))
        println(cuadrado(4))
        println(esPar(7))

        // EJERCICIO 2
        val cubo: Int => Int = num => num*num*num
        val promedio: (Double, Double) => Double = (a,b) => (a+b)/2
        val mayor: (Int, Int) => Int = (a,b) => if a > b then a else b
        val esMultiploDeTres: Int => Boolean = num => num%3 == 0
        println()
        println(cubo(2))
        println(promedio(7,5))
        println(mayor(7,4))
        println(esMultiploDeTres(243))

        //EJERCICIO 3
        println()
        println(transformar(5, duplicar))
        println(transformar(5, cuadrado))
        println(transformar(5, x => x +10))
        println(transformar(5, x => x * (-1)))  

        //EJERCICIO 4
        println()
        println(calcular(6.7,7.6,(a,b)=> a+b))
        println(calcular(8,2,(a,b)=>a-b))
        println(calcular(3.1,4.7,(a,b)=>a*b))
        println(calcular(2.2,2,(a,b)=>a/b))
        println(calcular(7,5,(a,b)=> (a+b)/2))




        //EJERCICI0 5
        val duplicar1 = crearMultiplicador(2)
        val triplicar = crearMultiplicador(3)
        val porDiez: Int => Int = crearMultiplicador(10)
        println()
        println(duplicar1(2))
        println(triplicar(3))
        println(porDiez(5))


        //EJERCICIO 6
        val descuento10: Double => Double = precio => aplicarDescuento(precio, 0.1)
        val descuento20: Double => Double = precio => aplicarDescuento(precio, 0.2)
        val descuento50: Double => Double = precio => aplicarDescuento(precio, 0.5)
        println()
        println(descuento10(450))
        println(descuento20(600))
        println(descuento50(700))

        // EJERCICIO 7
        val cuadradoCurry = elevar(2)
        val cuboCurry = elevar(3)
        val cuartaPotencia = elevar(4)
        println()
        println(cuadradoCurry(6))
        println(cuboCurry(3))
        println(cuartaPotencia(2))


         //EJERCICIO 8
        val limpiar: String => String = pal => if pal.head == ' ' then {if pal.last == ' ' then pal.substring(1,pal.length()-1) else pal.tail} else {if pal.last == ' ' then pal.init else pal} 
        val convertirMinusculas: String => String = pal => pal.toLowerCase()
        val longitud: String => Int = pal => pal.length()
        val esLongitudValida: Int => Boolean = num => num>5
        val validarTexto: String => Boolean = pal => limpiar.andThen(convertirMinusculas).andThen(longitud).andThen(esLongitudValida)(pal)
        println()
        println(validarTexto(" hola soy jose"))

        //EJERCICIO 9
        println()
        println(sumar(1,4,x => x))
        println(sumar(1,4,x => x*x))
        println(producto(1,4,x => x))

        //EJERCICIO 10
        println()
        println(transformarTexto("scala", c => c.toUpper))
        println(buscarCaracter("Salz",c => c == 'z'))
        


    }
}