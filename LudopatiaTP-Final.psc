Funcion DarBienvenida(msj)
    Definir esperandoUsuario Como Caracter
	
	Borrar Pantalla
    Escribir "****************************************"
    Escribir "*               " msj "             *"
    Escribir "****************************************"
    Escribir "* ¡Bienvenido a la Ruleta Virtual!     *"
    Escribir "* Disfruta de la experiencia,          *"
    Escribir "* apuesta y prueba tu suerte.          *"
    Escribir "*                                      *"
    Escribir "* Si eres nuevo, regístrate.           *"
    Escribir "* Si ya tienes cuenta, inicia sesión.  *"
	Escribir "*                                      *"
	Escribir "*  Presione Enter para continuar       *"
	Escribir "****************************************"
	Leer esperandoUsuario
FinFuncion


Funcion opMenu <- MostrarMenu ( opMenu )
	Definir opcionMenu como caracter
	
	Borrar Pantalla
	Escribir "*******************************************"
	Escribir "*                  MENU                   *"
	Escribir "*******************************************"
	Escribir "*-> Seleccionar 1 para Registrarse        *"
	Escribir "*-> Seleccionar 2 para Iniciar Sesión     *"
	Escribir "*-> Seleccionar 3 para Salir              *"
	Escribir "*******************************************"
	Leer opcionMenu
	
	pudeSeguir <- verificoNum (opcionMenu)
	
	Mientras (opcionMenu <> "1") y (opcionMenu <> "2") y (opcionMenu <> "3") y (pudeSeguir = 1) Hacer
		Escribir "La opcion que ingresó es incorrecto, favor de ingresar nuevamente: "
		Leer opcionMenu
	Fin Mientras
	
	opMenu <- ConvertirANumero(opcionMenu)
Fin Funcion

Funcion pedirDatosRegistro(usuarioNombre, usuarioDNI, usuarioContrasenia, cont)
	Definir repetirContrasenia , numDNI Como caracter
	Definir flag, DNI Como Entero

	Borrar Pantalla
	Escribir "*******************************************"
	Escribir "*                REGISTRO                 *"
	Escribir "*******************************************"
	Escribir "* Favor de ingresar su nombre de usuario: *"
	Leer usuarioNombre[cont]
	
	flag <- 0
	
	Repetir
		Escribir "*   Por favor, ingrese su número de DNI   *"
		Escribir "*   (Siendo desde 10 millones hasta 60    *"
		Escribir "*   millones), sin puntos ni comas.       *"
		Escribir "*   Ejemplo: 67890123                     *"

		Leer numDNI
		
		pudeSeguir <- verificoNum (numDNI) 
		
		si pudeSeguir  = 0
			DNI <- convertirAnumero(numDNI)
			Para i<- 0 Hasta 2 Con Paso 1 Hacer
				si DNI = usuarioDNI[i]
					Escribir "->  DNI NO DISPONIBLE "
					flag <- 1
					i <- 2
					Esperar 1 Segundos
				SiNo
					si (i = 2) y (DNI <> usuarioDNI[i])
						flag <- 0
						usuarioDNI[cont] <- DNI
					FinSi
				FinSi
			Fin Para
		SiNo
			flag <- 1
			Escribir "*   Las letras no son válidas para el DNI *"
		FinSi
		
	Hasta Que ((usuarioDNI[cont] >= 1000000) y (usuarioDNI[cont] <= 60000000)) y (flag = 0)
	
	Escribir "*   Por favor, ingrese su contraseña:     *"
	Escribir "*   (Debe recordar que mínimo son 5       *"
	Escribir "*   caracteres)                           *"

	Leer usuarioContrasenia[cont]
	
	Mientras Longitud(usuarioContrasenia[cont]) <= 4  Hacer
		Escribir "*  La contraseña es muy corta, tiene que  *"
		Escribir "*  tener minimo 5 caracteres. Por favor,  *"
		Escribir "*  ingrese su contraseña:                 *"
		Leer usuarioContrasenia[cont]
	Fin Mientras
	
	Escribir "*  Por favor, vuelva a ingresar su        *"
	Escribir "*  contraseña:                            *"

	Leer repetirContraseña
	
	Mientras usuarioContrasenia[cont] <> repetirContraseña Hacer
		Escribir "*  Las contraseñas no son iguales         *"
		Escribir "*  Por favor, reingrese su contraseña:    *"
		Leer usuarioContrasenia[cont]
		Escribir "*  Por favor, vuelva a ingresar su        *"
		Escribir "*  contraseña:                            *"
		Leer repetirContraseña
	Fin Mientras
	
	Escribir "*******************************************"
	Escribir "*             REGISTRO COMPLETO           *"
	Escribir "*******************************************"
Fin Funcion

Funcion idUser <- pedirDatosInicioSesion (usuarioDNI, usuarioContrasenia)
	Definir numDNI, opSiNo como caracter
	Definir DNI, intentos Como Entero
	
	// Pide los datos al usuario para ingresar con cuenta
	intentos <- 3
	
	Repetir
		Borrar Pantalla
		Escribir "*******************************************"
		Escribir "*             INICIO SESION               *"
		Escribir "*******************************************"
		Escribir "*   Por favor, ingrese su número de DNI:  *"

		Leer numDNI

		pudeSeguir <- verificoNum (numDNI)
		//verifica que sea numerico
		
		si pudeSeguir  = 0
			DNI <- ConvertirANumero(numDNI)
			Para i <- 0 Hasta 2 Con Paso 1 Hacer
				si DNI = usuarioDNI[i] Entonces
					Escribir "*******************************************"
					Escribir "*            USUARIO ENCONTRADO           *"
					Escribir "*******************************************"
					
					Repetir
						Escribir "*   Por favor, ingrese su contraseña:     *"
						Leer contrasenia
						
						si contrasenia = usuarioContrasenia[i]
							Borrar Pantalla
							Escribir "*******************************************"
							Escribir "*           CONTRASEÑA CORRECTA           *"
							Escribir "*******************************************"
							Esperar 1 Segundos
							idUser <- i
							i <- 2
						SiNo
							Borrar Pantalla
							intentos <- intentos - 1
							Escribir "*******************************************"
							Escribir "*   CLAVE INCORRECTA, INTENTE NUEVAMENTE  *"
							Escribir "*   CANTIDAD DE INTENTOS: ", intentos, "               *"
							Escribir "*******************************************"

							idUser <- 99
						FinSi
					Hasta Que (intentos = 0) o (idUser >= 0 y IdUser < 4)
					//le asigno 3 para que salga del bucle, y no siga buscando usuarios
					i <- 2
				SiNo
					Si i = 2 Entonces
						Borrar Pantalla
						Escribir "*******************************************"
						Escribir "*        NO SE ENCONTRÓ AL USUARIO        *"
						Escribir "*******************************************"
						Esperar 1 Segundos
						idUser <- 99
					FinSi
				FinSi
			Fin Para
		SiNo
			Escribir "-> Incorrecto"
			idUser <- 99
		FinSi
	Hasta Que ((idUser >= 0 y IdUser < 4) o (idUser = 99))
Fin Funcion

Funcion opMenu <- MostrarSegundoMenu ( opMenu, usuarioNombre )
	Definir opcionMenu Como Caracter
	Borrar Pantalla
	Escribir "*******************************************"
	Escribir "*                  MENU                   *"
	Escribir "*******************************************"
	Escribir "*-> Seleccionar 1 para Cargar moneda      *"
	Escribir "*-> Seleccionar 2 para Jugar Rullette     *"
	Escribir "*-> Seleccionar 3 para mostrar reglas     *"
	Escribir "*-> Seleccionar 4 ver mi billetera        *"
	Escribir "*-> Seleccionar 5 cerrar sesión           *"
	Escribir "*******************************************"
	
	Leer opcionMenu
	
	pudeSeguir <- verificoNum (opcionMenu)
	
	Mientras (opcionMenu <> "1") y (opcionMenu <> "2") y (opcionMenu <> "3") y (opcionMenu <> "4") y (pudeSeguir = 1) Hacer
		Escribir "La opcion que ingresó es incorrecto, favor de ingresar nuevamente: "
		Leer opcionMenu
		pudeSeguir <- verificoNum (opcionMenu)
	Fin Mientras
	
	opMenu <- ConvertirANumero(opcionMenu)
Fin Funcion


Funcion CargarPlataAlUsuario(usuarioPlata, usuarioDNI)
	Definir DNI, depositoUsuario, flag2 como entero
	Definir opSiNo, deposito, DNIusuario Como caracter
	
	// Preguntar q dni quiere cargar plata
	Borrar Pantalla
	Escribir "*******************************************"
	Escribir "*             CARGA MONETARIA             *"
	Escribir "*******************************************"

	//buscar cuenta 
	Repetir
		Escribir "*   ¿A qué DNI le quiere cargar plata?    *"
		Leer DNIusuario
		pudeSeguir <- verificoNum (DNIusuario)
		
		Mientras pudeSeguir = 1 Hacer
			Escribir "*   DNI incorrecto, ingrese nuevamente.   *"
			leer DNIusuario
			pudeSeguir <- verificoNum (DNIusuario)
		Fin Mientras
		
		DNI <- ConvertirANumero(DNIusuario)
		
		Para i<- 0 Hasta 2 Con Paso 1 Hacer
			si DNI = usuarioDNI[i]
				Escribir "*   DNI correcto, favor de ingresar el    *"
				Escribir "*   monto a depositar.                    *"
				Leer deposito
				pudeSeguir <- verificoNum (deposito) 
				
				Mientras pudeSeguir = 1 y depositoUsuario < 1 Hacer
					Escribir "*   El monto que ingresó es incorrecto,   *"
					Escribir "*   favor de ingresar el monto nuevamente *"
					Leer deposito
					pudeSeguir <- verificoNum (deposito)
				Fin Mientras
				
				depositoUsuario <- ConvertirANumero(deposito)
				
				usuarioPlata[i]<- usuarioPlata[i] + depositoUsuario
				Escribir "*******************************************"
				Escribir "*       PLATA CARGADA CORRECTAMENTE       *"
				Escribir "*   MONTO CARGADO: $ ", usuarioPlata[i]
				Escribir "*******************************************"
				Esperar 1 Segundos
				i <- 2
			SiNo
				si i = 2 y DNI <> usuarioDNI[i]
					Escribir "->  DNI NO ENCONTRADO "
					Esperar 1 Segundos
				FinSi
			FinSi
		Fin Para
		
		Repetir
			Escribir "*   ¿Quiere seguir cargando plata?        *"
			Escribir "*   0 -> Si                               *"
			Escribir "*   1 -> No                               *"

			Leer opSiNo
			pudeSeguir <- verificoNum (opSiNo) 
		Hasta Que pudeSeguir = 0
		
		flag2 <- convertirAnumero(opSiNo)
		
	Hasta Que flag2 = 1
Fin Funcion


Funcion CargarNumerosRuleta (ruleta, totalPosiciones)
	// Carga el array con datos
	Para i <- 0 Hasta totalPosiciones - 1 Con Paso 1 Hacer
		ruleta[i] <- i
	Fin Para
Fin Funcion


Funcion posicionCaer <- MostrarRuleta(ruleta, totalPosiciones, posicionInicial, vueltas, posicionCaer, velocidadPelotita)
	//************************************ Mostrar ruleta ************************************
	// Generar dos posiciones aleatorias, una para el comienzo y otra para el final
    posicionInicial <- Azar(totalPosiciones)
	posicionCaer <- Azar(totalPosiciones)
    
	// Definir la cantidad de vueltas completas (al menos 3)
    vueltas <- 3 + Azar(5)
	
	// Simulación del giro de la ruleta, empezando por la posicion inicial 
    Para i <- 0 Hasta ((vueltas * totalPosiciones) + posicionCaer) Con Paso 1 Hacer
        Borrar Pantalla
        Escribir "*******************************************"
		Escribir "*                 RULETTE                 *"
		Escribir "*******************************************"

		Escribir ""
		Si	posicionInicial < 10 Entonces
			Escribir "*... POSICION INICIAL:    0", posicionInicial, " ...*"
		SiNo
			Escribir "*... POSICION INICIAL:    ", posicionInicial, " ...*"
		FinSi
		
		Escribir "*... CANTIDAD DE VUELTAS: 0", vueltas, " ...*"
		Escribir ""
		// Mostrar la ruleta con la posición actual resaltada
        Para j <- 0 Hasta totalPosiciones - 1 Con Paso 1 Hacer
            Si (posicionInicial + i) Mod totalPosiciones = j Entonces
				posicionCaer <- ruleta[j]
				Si j=0 Entonces
					Escribir "|||[0", ruleta[j], "]|||"
				SiNo 
					Si j <= 9 Entonces
						Si ((j mod 3) = 0) Entonces
							Escribir "[0", ruleta[j], "]"
						SiNo 
							Escribir Sin Saltar "[", ruleta[j], "]"	
						Fin Si
					Sino 
						Si j >= 10 Entonces
							Si ((j mod 3) = 0) Entonces
								Escribir "[", ruleta[j], "]"
							SiNo 
								Escribir Sin Saltar "[", ruleta[j], "]"	
							Fin Si
						fin si
					Fin Si
				Fin Si
            SiNo
				Si j=0 Entonces
					Escribir  "||||0", ruleta[j], "||||"
				SiNo 
					Si j <= 9 Entonces
						Si ((j mod 3) = 0) Entonces
							Escribir "|0",ruleta[j],"|"
						SiNo 
							Escribir Sin Saltar "|0", ruleta[j]	
						Fin Si
					Sino 
						Si j >= 10 Entonces
							Si ((j mod 3) = 0) Entonces
								Escribir "|",ruleta[j],"|"
							SiNo 
								Escribir Sin Saltar "|", ruleta[j]	
							Fin Si
						fin si
					Fin Si
				Fin Si
            Fin Si
        Fin Para
        
		// Esperar un poco para simular el giro
        Esperar (velocidadPelotita) Milisegundos
		
		// Disminuir la velocidad de la pelota
		Si velocidadPelotita <> 0 Entonces
			velocidadPelotita <- velocidadPelotita + 0.5
		FinSi
    Fin Para
Fin Funcion

Funcion MostrarReglas(msj)
	Definir opMensaje Como Entero
	Definir mensaje Como Caracter
	
    Repetir
		Repetir
			Borrar Pantalla
			Escribir "*******************************************"
			Escribir "*                 REGLAS                  *"
			Escribir "*******************************************"
			Escribir "*   ¡Bienvenido a la guía de reglas!      *"
			Escribir "*   Aquí encontrarás todo lo necesario    *"
			Escribir "*   para jugar a la ruleta.               *"
			Escribir "*******************************************"
			Escribir "*   Seleccione una opción para aprender:  *"
			Escribir "*   1 -> Objetivo del juego               *"
			Escribir "*   2 -> Cómo apostar                     *"
			Escribir "*   3 -> Tipos de apuestas                *"
			Escribir "*   4 -> Probabilidades y ganancias       *"
			Escribir "*   5 -> Como jugar responsablemente      *"
			Escribir "*   6 -> Salir                            *"
			Escribir "*******************************************"
			
			Leer mensaje
			pudeSeguir <- verificoNum (mensaje) 
		Hasta Que pudeSeguir = 0
		
		opMensaje <- convertirAnumero(mensaje)
		
        Segun opMensaje Hacer
            caso 1:
                Escribir "*******************************************"
                Escribir "*         OBJETIVO DEL JUEGO              *"
                Escribir "*   La ruleta es un juego de azar donde   *"
                Escribir "*   debes predecir dónde caerá la bola.   *"
                Escribir "*   Las opciones incluyen números         *"
                Escribir "*   individuales, colores y columnas.     *"
                Escribir "*                                         *"
                Escribir "*   ¡Mientras más específica sea tu       *"
                Escribir "*   apuesta, mayor será la recompensa!    *"
                Escribir "*******************************************"
            caso 2:
                Escribir "*******************************************"
                Escribir "*             CÓMO APOSTAR                *"
                Escribir "*   1. Decide cuánto dinero quieres jugar *"
                Escribir "*      (mínimo $100).                     *"
                Escribir "*   2. Elige tu tipo de apuesta:          *"
                Escribir "*      - Número específico                *"
                Escribir "*      - Color: Rojo o Negro              *"
                Escribir "*      - Columna o docena                 *"
                Escribir "*   3. Confirma tu apuesta y observa cómo *"
                Escribir "*      gira la ruleta.                    *"
                Escribir "*******************************************"
            caso 3:
                Escribir "*******************************************"
                Escribir "*          TIPOS DE APUESTAS              *"
                Escribir "*   -> Números individuales: Pagas por    *"
                Escribir "*      un número específico.              *"
                Escribir "*   -> Color: Rojo o Negro.               *"
                Escribir "*   -> Columnas: Primera, segunda o       *"
                Escribir "*      tercera columna.                   *"
                Escribir "*   -> Docenas: Grupos de 12 números      *"
                Escribir "*      consecutivos.                      *"
                Escribir "*   -> Par o Impar: Números pares o       *"
                Escribir "*      impares.                           *"
                Escribir "*   Ejemplo:                              *"
                Escribir "*   Apostaste $200 al número 7. Si ganas, *"
                Escribir "*   recibirás $7000.                      *"
                Escribir "*******************************************"
            caso 4:
                Escribir "*******************************************"
                Escribir "*      PROBABILIDADES Y GANANCIAS         *"
                Escribir "*   -> Número específico: 2.7% de éxito.  *"
                Escribir "*      Ganancia: 35 veces tu apuesta.     *"
                Escribir "*   -> Color: 48.6% de éxito.             *"
                Escribir "*      Ganancia: 1 vez tu apuesta.        *"
                Escribir "*   -> Columnas y Docenas: 32.4% de éxito.*"
                Escribir "*      Ganancia: 2 veces tu apuesta.      *"
                Escribir "*                                         *"
                Escribir "*   Recuerda que la banca siempre tiene   *"
                Escribir "*   ventaja (la casilla 0 y 00).          *"
                Escribir "*******************************************"
            caso 5:
                Escribir "*******************************************"
                Escribir "*  CONSEJOS PARA JUGAR RESPONSABLEMENTE   *"
                Escribir "*   1. Establece un presupuesto y no lo   *"
				Escribir "*      superes, respétalo.                *"
                Escribir "*   2. No intentes recuperar pérdidas.    *"
                Escribir "*   3. Juega por diversión, no como un    *"
                Escribir "*      medio para ganar dinero.           *"
                Escribir "*   4. Tómate descansos y no apuestes     *"
                Escribir "*      dinero que no puedes permitirte    *"
                Escribir "*      perder.                            *"
                Escribir "*   5. Recuerda: Las probabilidades están *"
                Escribir "*      a favor de la banca.               *"
                Escribir "*******************************************"
            caso 6:
                Escribir "*******************************************"
                Escribir "*         GRACIAS POR CONSULTAR           *"
                Escribir "*   ¡Buena suerte en tus próximas jugadas!*"
                Escribir "*   ¡Recuerda jugar responsablemente!     *"
                Escribir "*******************************************"
            De Otro Modo:
                Escribir "*******************************************"
                Escribir "*      Opción no válida, inténtelo        *"
                Escribir "*               nuevamente.               *"
                Escribir "*******************************************"
        Fin Segun
		
		Escribir "*    presione enter para continuar.       *"
		Leer mensaje
		
    Hasta Que opMensaje = 6
FinFuncion


Funcion pudeSeguir <- verificoNum (numA)
	//Defino la cadena de numeros
	Definir numB Como Caracter
	numB <- "0123456789"
	
    //Comparo cada numero con el caracter de A
	Para j <- 0 Hasta Longitud(numA) Con Paso 1 Hacer
		Para k <- 0 Hasta Longitud(numB) Con Paso 1 Hacer
			// Si la cadena no tiene nada, termina el bucle
			si Subcadena(numA,j,j) = ""
				k <- Longitud(numB)
			SiNo
				//comparar el carcarter con cada número, 0 al 9
				Si Subcadena(numA,j,j) = Subcadena(numB,k,k)
					k <- Longitud(numB)
					pudeSeguir <- 0
				SiNo
					//Encontró un caracter NO NUMÉRICO, termina el bucle
					si k = Longitud(numB) Entonces
						j <- Longitud(numA)
					FinSi
					pudeSeguir <- 1
				FinSi
			FinSi
		FinPara
	FinPara
FinFuncion


Funcion apuestaEntero <- usuarioApuestaFicha(plataJugadaE, apuestaEnteroRuleta)
	Definir apuesta_Num_Color_Quincena Como Caracter
	Borrar Pantalla
	Escribir "*******************************************"
	Escribir "*   Usted apostó, $ " plataJugadaE
	Escribir "*******************************************"
	Escribir "* 01 - Al Rojo                            *"
	Escribir "* 02 - Al negro                           *"
	Escribir "* 03 - Par                                *"
	Escribir "* 04 - Impar                              *"
	Escribir "* 05 - Falta (del 1 al 18)                *"
	Escribir "* 06 - Pasa (del 19 al 36)                *"
	Escribir "* 07 - A la primera Docena (del 1 al 12)  *"
	Escribir "* 08 - A la segunda Docena (del 13 al 24) *"
	Escribir "* 09 - A la tercera Docena (del 25 al 36) *"
	Escribir "* 10 - Columna 1                          *"
	Escribir "* 11 - Columna 2                          *"
	Escribir "* 12 - Columna 3                          *"
	Escribir "* 13 - Pleno (jugar un número específico) *"
	Escribir "*******************************************"
	
	Repetir
		Escribir "*   ¿A qué quiere apostar?                *"
		Leer apuesta_Num_Color_Quincena
		pudeSeguir <- verificoNum (apuesta_Num_Color_Quincena)
		
		Mientras pudeSeguir = 1 Hacer
			Escribir "Apuesta incorrecta, vuelve a igresar la apuesta: "
			leer apuesta_Num_Color_Quincena
			pudeSeguir <- verificoNum (apuesta_Num_Color_Quincena)
		Fin Mientras
		
		apuestaEntero <- ConvertirANumero(apuesta_Num_Color_Quincena)
	Hasta Que (apuestaEntero >= 1 y apuestaEntero <= 13)
	
FinFuncion

Funcion UsuarioGanoPerdio(plataJugadaE, apuestaEntero, apuestaEnteroRuleta, posicionCaer, idUser, usuarioPlata , usuarioNombre, numerosRojos, numerosNegros, columna1, columna2, columna3)
	Escribir ""
	Escribir "*******************************************"
	si posicionCaer < 10 Entonces
		Escribir "*   El número que salió es: 0" posicionCaer "        *"
	SiNo
		Escribir "*   El número que salió es: " posicionCaer "         *"
	FinSi
	
	Segun apuestaEntero Hacer
		caso 1:
			//	gana x 37
			//	"01 - Al Rojo"
			Para i<-0 Hasta 17 Con Paso 1 Hacer
				Si posicionCaer = numerosRojos[i] Entonces
					plataJugadaE <- plataJugadaE * 2
					MensajePerdioGano(idUser, plataJugadaE, 0, usuarioNombre)
					usuarioPlata[idUser] <- usuarioPlata[idUser] + plataJugadaE
					i<-17
				SiNo
					si i = 17
						MensajePerdioGano(idUser, plataJugadaE, 1, usuarioNombre)
					FinSi
				FinSi
			Fin Para
		caso 2:
			//	"02 - Al negro"
			Para i<-0 Hasta 17 Con Paso 1 Hacer
				Si posicionCaer = numerosNegros[i] Entonces
					plataJugadaE <- plataJugadaE * 2
					MensajePerdioGano(idUser, plataJugadaE, 0, usuarioNombre)
					usuarioPlata[idUser] <- usuarioPlata[idUser] + plataJugadaE
					i<-17
				SiNo
					si i = 17
						MensajePerdioGano(idUser, plataJugadaE, 1, usuarioNombre)
					FinSi
				FinSi
			Fin Para
		caso 3:
			//	"03 - Par"
			Si (posicionCaer % 2 = 0) Entonces
				plataJugadaE <- plataJugadaE * 2
				MensajePerdioGano(idUser, plataJugadaE, 0, usuarioNombre)
				usuarioPlata[idUser] <- usuarioPlata[idUser] + plataJugadaE
			SiNo
				MensajePerdioGano(idUser, plataJugadaE, 1, usuarioNombre)
			FinSi
		caso 4:
			//	"04 - Impar"
			Si (posicionCaer % 2 <> 0) Entonces
				plataJugadaE <- plataJugadaE * 2
				MensajePerdioGano(idUser, plataJugadaE, 0, usuarioNombre)
				usuarioPlata[idUser] <- usuarioPlata[idUser] + plataJugadaE
			SiNo
				MensajePerdioGano(idUser, plataJugadaE, 1, usuarioNombre)
			FinSi
		caso 5:
			//	"05 - Falta (del 1 al 18)"
			Si (posicionCaer > 0 y posicionCaer < 19) Entonces
				plataJugadaE <- plataJugadaE * 2
				MensajePerdioGano(idUser, plataJugadaE, 0, usuarioNombre)
				usuarioPlata[idUser] <- usuarioPlata[idUser] + plataJugadaE
			SiNo
				MensajePerdioGano(idUser, plataJugadaE, 1, usuarioNombre)
			FinSi
		caso 6:
			// 	"06 - Pasa (del 19 al 36)"
			Si (posicionCaer > 18 y posicionCaer < 37) Entonces
				plataJugadaE <- plataJugadaE * 2
				MensajePerdioGano(idUser, plataJugadaE, 0, usuarioNombre)
				usuarioPlata[idUser] <- usuarioPlata[idUser] + plataJugadaE
			SiNo
				MensajePerdioGano(idUser, plataJugadaE, 1, usuarioNombre)
			FinSi
		caso 7:
			//	"07 - A la primera Docena (del 1 al 12) "
			Si (posicionCaer > 0 y posicionCaer < 13) Entonces
				plataJugadaE <- plataJugadaE * 3
				MensajePerdioGano(idUser, plataJugadaE, 0, usuarioNombre)
				usuarioPlata[idUser] <- usuarioPlata[idUser] + plataJugadaE
			SiNo
				MensajePerdioGano(idUser, plataJugadaE, 1, usuarioNombre)
			FinSi
		caso 8:
			//	"08 - A la segunda Docena (del 13 al 24) "
			Si (posicionCaer > 12 y posicionCaer < 25) Entonces
				plataJugadaE <- plataJugadaE * 3
				MensajePerdioGano(idUser, plataJugadaE, 0, usuarioNombre)
				usuarioPlata[idUser] <- usuarioPlata[idUser] + plataJugadaE
			SiNo
				MensajePerdioGano(idUser, plataJugadaE, 1, usuarioNombre)
			FinSi
		caso 9:
			//	"09 - A la tercera Docena (del 25 al 36) "
			Si (posicionCaer > 24 y posicionCaer < 37) Entonces
				plataJugadaE <- plataJugadaE * 3
				MensajePerdioGano(idUser, plataJugadaE, 0, usuarioNombre)
				usuarioPlata[idUser] <- usuarioPlata[idUser] + plataJugadaE
			SiNo
				MensajePerdioGano(idUser, plataJugadaE, 1, usuarioNombre)
			FinSi
		caso 10:
			//	"12 - Columna 1"
			Para i<-0 Hasta 11 Con Paso 1 Hacer
				Si posicionCaer = columna1[i] Entonces
					plataJugadaE <- plataJugadaE * 3
					MensajePerdioGano(idUser, plataJugadaE, 0, usuarioNombre)
					usuarioPlata[idUser] <- usuarioPlata[idUser] + plataJugadaE
					i<-11
				SiNo
					si i = 11
						MensajePerdioGano(idUser, plataJugadaE, 1, usuarioNombre)
					FinSi
				FinSi
			Fin Para
		caso 11:
			//	"12 - Columna 2"
			Para i<-0 Hasta 11 Con Paso 1 Hacer
				Si posicionCaer = columna2[i] Entonces
					plataJugadaE <- plataJugadaE * 3
					MensajePerdioGano(idUser, plataJugadaE, 0, usuarioNombre)
					usuarioPlata[idUser] <- usuarioPlata[idUser] + plataJugadaE
					i<-11
				SiNo
					si i = 11
						MensajePerdioGano(idUser, plataJugadaE, 1, usuarioNombre)
					FinSi
				FinSi
			Fin Para
		caso 12:
			//	"12 - Columna 3"
			Para i<-0 Hasta 11 Con Paso 1 Hacer
				Si posicionCaer = columna3[i] Entonces
					plataJugadaE <- plataJugadaE * 3
					MensajePerdioGano(idUser, plataJugadaE, 0, usuarioNombre)
					usuarioPlata[idUser] <- usuarioPlata[idUser] + plataJugadaE
					i<-11
				SiNo
					si i = 11
						MensajePerdioGano(idUser, plataJugadaE, 1, usuarioNombre)
					FinSi
				FinSi
			Fin Para
		caso 13:
			//	"13 - Pleno (jugar un número específico)"
			Si (posicionCaer = apuestaEnteroRuleta) Entonces
				plataJugadaE <- plataJugadaE * 36
				MensajePerdioGano(idUser, plataJugadaE, 0, usuarioNombre)
				usuarioPlata[idUser] <- usuarioPlata[idUser] + plataJugadaE
			SiNo
				MensajePerdioGano(idUser, plataJugadaE, 1, usuarioNombre)
			FinSi
		De Otro Modo:
			Escribir "equivocado"
	Fin Segun
FinFuncion

Funcion CrearDimensionesParaEvaluarJugada(numerosRojos, numerosNegros, columna1, columna2, columna3)
    Definir fila1, fila2, fila3 Como Entero
	Definir num1,num2 Como Entero
	num1<- 0
	num2<- 0
	Para i <- 1 Hasta 36 Hacer
        // Verificar si el número es rojo o negro
        Si (i = 1 O i = 3 O i = 5 O i = 7 O i = 9 O i = 12 O i = 14 O i = 16 O i = 18 O i = 19 O i = 21 O i = 23 O i = 25 O i = 27 O i = 30 O i = 32 O i = 34 O i = 36) Entonces
			numerosRojos[num1] <- i
			num1 <- num1 + 1
		SiNo
			numerosNegros[num2] <- i
			num2 <- num2 + 1
        Fin Si
    Fin Para
	
    fila1 <- 0
    fila2 <- 0
    fila3 <- 0
	
	Para i <- 1 hasta 36 Con Paso 1 Hacer
        // Clasificar según la columna a la que pertenece
		Si (i MOD 3 = 1) Entonces
			columna1[fila1] <- i
			fila1 <- fila1 + 1
		Sino 
			Si (i MOD 3 = 2) Entonces
				columna2[fila2] <- i
				fila2 <- fila2 + 1
			Sino
				columna3[fila3] <- i
				fila3 <- fila3 + 1
			FinSi
		FinSi
	FinPara
	
FinFuncion

Funcion MensajePerdioGano(idUser, plataJugadaE, GanPer, usuarioNombre)
	Si GanPer = 0 Entonces
		Escribir "*******************************************"
		Escribir "*   Felicitaciones, " usuarioNombre[idUser] "!"
		Escribir "*   Ganaste $" plataJugadaE " en tu apuesta. "
		Escribir "*   Sigue así y prueba tu suerte.         *"
		Escribir "*******************************************"
	SiNo
		Escribir "*******************************************"
		Escribir "*   Lamentamos tu pérdida, " usuarioNombre[idUser] "."
		Escribir "*   Perdió $" plataJugadaE " esta vez."
		Escribir "*   ¡Intente nuevamente y buena suerte!   *"
		Escribir "*******************************************"
	FinSi
FinFuncion

Funcion VerBilleteraDelUsuario(usuarioPlata, i)
	Si usuarioPlata[i] >= 100 Entonces
		Escribir "*******************************************"
		Escribir "*   Su saldo actual es $" usuarioPlata[i] "."
		Escribir "*   ¡Suficiente para seguir jugando!      *"
		Escribir "*******************************************"
	SiNo
		Escribir "*******************************************"
		Escribir "*   Su saldo actual es $" usuarioPlata[i] "."
		Escribir "*   Recargue para seguir disfrutando.     *"
		Escribir "*******************************************"
	FinSi
FinFuncion

//*************************************************************************************************************************************************************************************************

Algoritmo Ludopatía
	Dimension ruleta[37], usuarioNombre[3], usuarioDNI[3], usuarioContrasenia[3], usuarioPlata[3], numerosRojos[18], numerosNegros[18], columna1[12], columna2[12], columna3[12]
    Definir cont, j ,posicionInicial, totalPosiciones, vueltas, posicionCaer, opMenu, DNI, bienMal, idUser, pudeSeguir, plataJugadaE, apuestaEntero, opSiNo, aux, ErrorSesion, apuestaEnteroRuleta Como Entero
	Definir intervalo , velocidadPelotita como Real
	Definir nombre, apellido, contrasenia, opcionSiNo, plataJugadaC, esperandoUsuario, num0al36 Como Caracter
	cont <- 0
	totalPosiciones <- 37
	velocidadPelotita <- 1
	ErrorSesion <- 1
	
	CrearDimensionesParaEvaluarJugada(numerosRojos, numerosNegros, columna1, columna2, columna3)
	
	DarBienvenida("Bienvenido")
	
	// Funcion que carga los datos de la ruleta (los numeros del 0 al 37)
	CargarNumerosRuleta(ruleta, totalPosiciones)
	
	Repetir
		opMenu <- MostrarMenu(opMenu)
		//Logear o registrarse
		Si opMenu = 1 y cont < 3 Entonces
			pedirDatosRegistro(usuarioNombre, usuarioDNI, usuarioContrasenia, cont)
			idUser <- cont
			cont <- cont + 1
			aux <- idUser
			ErrorSesion <- 0
		SiNo
			Si opMenu = 1 y cont = 3 Entonces
				ErrorSesion <- 1
				Escribir "->> Base de Datos llena, Favor "
				Escribir "    de reiniciar el programa "
				Esperar 1 Segundos
			FinSi
			Si opMenu = 2 Entonces
				Repetir
					idUser <- pedirDatosInicioSesion(usuarioDNI, usuarioContrasenia)
					Si idUser = 99 Entonces
						Repetir
							Escribir "*   Error en el inicio de sesión.         *"
							Escribir "*   Revise su DNI o contraseña.           *"
							Escribir "*******************************************"
							Escribir "*   ¿Desea seguir intentando?             *"
							Escribir "*   0 -> SI                               *"
							Escribir "*   1 -> NO                               *"
							Leer opcionSiNo
							ErrorSesion <- 1
							idUser <- aux
							pudeSeguir <- verificoNum (opcionSiNo)
							opMenu <- ConvertirANumero(opcionSiNo)
						Hasta Que pudeSeguir = 0 
					SiNo
						Escribir "*   Bienvenido(a) de nuevo, " usuarioNombre[idUser] "."
						Escribir "*   ¡Que tengas buena suerte!             *"
						Escribir "*******************************************"
						ErrorSesion <- 0
					FinSi
				Hasta Que (ErrorSesion = 0) o (opMenu = 1)
			FinSi
		FinSi
		
		Si (ErrorSesion <> 1) Entonces
			opMenu <- 0
			Esperar 1 Segundos
			
			Repetir
				//Preguntar lo que quiere hacer al usuario
				opMenu <- MostrarSegundoMenu(opMenu, usuarioNombre)
				
				Segun opMenu Hacer
					caso 1:
						CargarPlataAlUsuario(usuarioPlata, usuarioDNI)
					caso 2:
						//validar juego de ejemplo
						Repetir
							Escribir "*   ¿Desea ver un juego de prueba?        *"
							Escribir "*            SI / NO                      *"
							Escribir "*   Recuerda que el monto minimo es $100  *"
							Leer opcionSiNo
							opcionSiNo <- Mayusculas(opcionSiNo)
						Hasta Que opcionSiNo = "SI" o opcionSiNo = "NO"
						
						Repetir
							Si opcionSiNo = "SI" Entonces
								// Funcion que muestra la ruleta en posicion del tablero
								posicionCaer <- MostrarRuleta(ruleta, totalPosiciones, posicionInicial, vueltas, posicionCaer, velocidadPelotita)
								Escribir ""
								Escribir "*******************************************"
								Escribir "*   ¡Posicion que cayó la pelota -> " posicionCaer
							sino
								
								//verifica que el usuario tenga plata en la billetera
								Si usuarioPlata[idUser] < 100
									Escribir "Usted tiene, $ ", usuarioPlata[idUser], " no puede jugar con ese monto, debe ser mayor a 100"
									Esperar 1 Segundos
								SiNo
									
									//preguntar cuanto quiere apostar y a qué numero/opcion, despues cargarlo
									Repetir
										Escribir "*   Usted tiene $ " usuarioPlata[idUser]
										Escribir "*   ¿Con cuánto desea jugar?            *"

										leer plataJugadaC
										pudeSeguir <- verificoNum (plataJugadaC)
										plataJugadaE <- ConvertirANumero(plataJugadaC)
									Hasta Que (pudeSeguir = 0) y (plataJugadaE <= usuarioPlata[idUser])
									
									apuestaEntero <- usuarioApuestaFicha (plataJugadaE, apuestaEnteroRuleta)
									
									si apuestaEntero = 13 Entonces
										Repetir
											Escribir "*   ¿A qué quiere número quiere jugar?    *"
											Leer num0al36
											pudeSeguir <- verificoNum (num0al36)
											
											Mientras pudeSeguir = 1 Hacer
												Escribir "Apuesta incorrecta, vuelve a igresar la apuesta: "
												leer num0al36
												pudeSeguir <- verificoNum (num0al36)
											Fin Mientras
											
											apuestaEnteroRuleta <- ConvertirANumero(num0al36)
										Hasta Que (apuestaEnteroRuleta >= 0 y apuestaEnteroRuleta <= 36)
									FinSi
									
									//restarle la plata al usuario
									usuarioPlata[idUser] <- usuarioPlata[idUser] - plataJugadaE
									
									// Funcion que muestra el moivimiento de la ruleta
									posicionCaer <- MostrarRuleta(ruleta, totalPosiciones, posicionInicial, vueltas, posicionCaer, velocidadPelotita)
									
									// Definir si gano o no el jugador
									UsuarioGanoPerdio(plataJugadaE, apuestaEntero, apuestaEnteroRuleta, posicionCaer, idUser, usuarioPlata , usuarioNombre, numerosRojos, numerosNegros, columna1, columna2, columna3)
									
								FinSi
							FinSi
							
							Si opcionSiNo = "SI"
								Escribir "*   Presione enter para continuar         *"
								Escribir "*******************************************"
								Leer esperandoUsuario
								opSiNo <- 1
							SiNo
								Repetir
									Escribir "*   ¿Desea seguir apostando?              *"
									Escribir "*   0 -> SI                               *"
									Escribir "*   1 -> NO                               *"
									Leer opcionSiNo
									pudeSeguir <- verificoNum (opcionSiNo)
								Hasta Que pudeSeguir = 0
								opSiNo <- ConvertirANumero(opcionSiNo)
							FinSi
						Hasta Que opSiNo = 1
					caso 3:
						//Reglas del jueg
						MostrarReglas(" ")
					caso 4:
						//Reglas del jueg
						VerBilleteraDelUsuario(usuarioPlata,idUser)
						Esperar 1 Segundos
					caso 5:
						Borrar Pantalla
						Escribir "*******************************************"
						Escribir "*                CERRANDO                 *"
						Escribir "*******************************************"
						Esperar 1 Segundos
						ErrorSesion <- 1
					De Otro Modo:
						//nunca entraría aqui, es por que no me deja usarlo vacío
						Escribir "Por favor, ingresar bien el dato!"
				Fin Segun
			Hasta Que (oPMenu = 5)
		FinSi
	Hasta Que opMenu = 3
	
	Escribir "*******************************************"
	Escribir "*   Gracias por jugar " usuarioNombre[idUser] "!"
	Escribir "*   Recuerda que siempre puedes mejorar   *"
	Escribir "*   tu suerte.                            *"
	Escribir "*   ¡Vuelve pronto para seguir jugando!   *"
	Escribir "*******************************************"
	
FinAlgoritmo
