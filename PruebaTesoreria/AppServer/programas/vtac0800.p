/*
    Empresa : ADOSA
    Programa: vtac0800.p
    Funcion : Correo electronico de confirmacion de alta de pedidos
    Autor   : ALEX
    Fecha   : 15 de Marzo del 2016
*/

//{/usr2/adosa/includes/sia00000.var}

DEFINE STREAM s-Salida.

DEFINE BUFFER b-detpedido FOR DetPedido.

DEFINE INPUT PARAMETER l-Pedido LIKE Pedido.Id-Pedido NO-UNDO.
DEFINE INPUT PARAMETER l-Resto LIKE Pedido.Resto NO-UNDO.

DEFINE NEW SHARED VARIABLE l-Arch2      AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-Arch1      AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-MailDe     AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-ResponderA AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-login      AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-Enviado    AS LOGICAL   NO-UNDO.  

DEFINE VARIABLE l-Archivo     AS CHARACTER.
DEFINE VARIABLE l-TipoEntrega AS CHARACTER NO-UNDO FORMAT "X(60)".
DEFINE VARIABLE l-CalleEmb    AS CHARACTER NO-UNDO FORMAT "X(60)".
DEFINE VARIABLE l-ColEmb      AS CHARACTER NO-UNDO FORMAT "X(60)".
DEFINE VARIABLE l-CdEmb       AS CHARACTER NO-UNDO FORMAT "X(60)".
DEFINE VARIABLE l-DirEmb      AS CHARACTER NO-UNDO FORMAT "X(60)".
DEFINE VARIABLE l-i           AS INTEGER.

DEFINE VARIABLE l-Asunto      LIKE e-Mail.Asunto    NO-UNDO.
DEFINE VARIABLE l-Contenido   LIKE e-Mail.Contenido NO-UNDO.

DEFINE VARIABLE l-Ciudad AS CHARACTER FORMAT 'x(60)' NO-UNDO.
DEFINE VARIABLE l-Estado AS CHARACTER FORMAT 'x(60)' NO-UNDO.
DEFINE VARIABLE l-Delegacion AS CHARACTER FORMAT 'x(60)' NO-UNDO.
DEFINE VARIABLE l-firma LIKE usuario.firma NO-UNDO.
DEFINE VARIABLE l-eMail LIKE Pedido.e-Mail NO-UNDO.

FIND Pedido WHERE Pedido.Id-Pedido = l-Pedido
              AND Pedido.Resto= l-Resto NO-LOCK NO-ERROR.
              
IF Pedido.Id-Vendedor = "0100" THEN
    RETURN.
              
FIND EstPedido WHERE EstPedido.Id-Pedido = l-Pedido
                 AND EstPedido.Id-Seq = l-Resto NO-LOCK NO-ERROR.
                 
FIND Cliente WHERE Cliente.id-cliente = pedido.id-cliente NO-LOCK NO-ERROR.

FIND CondVta WHERE CondVta.Id-Cond = Pedido.Id-Cond NO-LOCK NO-ERROR.

FIND Entrega WHERE Entrega.Id-Entrega = Pedido.Id-Entrega NO-LOCK NO-ERROR.

FIND Vendedor WHERE Vendedor.Id-Vendedor = Pedido.Id-Vendedor NO-LOCK NO-ERROR.
FIND FIRST Empleado WHERE Empleado.Iniciales = Vendedor.Iniciales NO-LOCK NO-ERROR.
FIND LAST Usuario WHERE Usuario.Id-User = Vendedor.Iniciales NO-LOCK NO-ERROR.
IF NOT AVAILABLE Usuario THEN 
    FIND LAST Usuario WHERE Usuario.Id-User = USERID("dictdb") NO-LOCK NO-ERROR. 

FIND FIRST Ciudad WHERE Ciudad.Id-Ciudad = Pedido.Id-Ciudad NO-LOCK NO-ERROR.
IF AVAILABLE ciudad THEN DO:
    ASSIGN l-Ciudad = Ciudad.Nombre.
    FIND FIRST Estado WHERE Estado.Id-Estado = Ciudad.Id-Estado NO-LOCK NO-ERROR.
    IF AVAILABLE Estado THEN
        ASSIGN l-Estado = Estado.Nombre.
END.

FIND cliente OF pedido NO-LOCK NO-ERROR.
IF AVAILABLE cliente THEN
    ASSIGN l-Delegacion = Cliente.Delegacion.

ASSIGN
    l-Archivo   = "/usr3/tmp/" + l-Pedido + "-" + TRIM(STRING(l-Resto,"99")) + ".lst"
    l-Asunto    = TRIM(Pedido.RazonSocial) + ", Confirmaci�n de pedido " + l-Pedido + "-" + TRIM(STRING(l-Resto,"99"))
    l-Contenido = "Hola " + TRIM(Pedido.RazonSocial) + ".<br/><br/>" +
                  "Agradecemos tu compra. En este correo encontrar�s un anexo en el cual puedes consultar a detalle tu pedido.<br/><br/>" +
                  "En caso de existir una modificaci�n o error, te pedimos contactar a " + 
                  (IF AVAILABLE Empleado THEN Empleado.Nombre ELSE "tu vendedor") +
                  " en el correo "+ 
                  (IF AVAILABLE Usuario AND Usuario.e-Mail <> "" THEN Usuario.e-Mail ELSE "ventas@adosa.com.mx") +
                  " o al tel�fono 8181581500, en la opci�n 7.<br/><br/>" +
                  '<p><span style="color:red;">IMPORTANTE: </span></p>' +
                  'Si elegiste recolectar tu "pedido en Bodega", <b>s�lo podr�s hacerlo una vez que hayas recibido la factura en tu correo</b>. Recuerda que debes presentar tu factura, de otro modo <b>no podr�s recolectarlo.</b>' 
    l-firma     = (IF AVAILABLE Usuario THEN Usuario.Firma ELSE "").

OUTPUT STREAM s-Salida TO VALUE(l-Archivo) PAGED PAGE-SIZE 40.
/* comentado para validar si envia lo de abajo por correo 
{/usr2/adosa/includes/cieheadr.i
    &Ancho     = 162
    &AnchoM    = 156
    &Titulo    = "'Confirmaci�n de alta de pedido ' + l-Pedido + '-' + TRIM(STRING(l-Resto))"
    &Subtitulo = "'Fecha de registro: ' + STRING(Pedido.FecReg,'99/99/9999')" 
    &Aveces    = "IF Pedido.ReqCte <> '' THEN ('Numero de requisici�n: ' + TRIM(STRING(Pedido.ReqCte))) ELSE ''"
    &STREAM    = s-Salida
}

*/  

IF (Pedido.Id-Entrega = 3 OR Pedido.Id-Entrega = 16) THEN DO:
    FIND Almacen WHERE Almacen.Id-Alm = Usuario.Id-Ubicacion NO-LOCK NO-ERROR.
    FIND Ciudad WHERE Ciudad.Id-Ciudad = Almacen.Id-Ciudad NO-LOCK NO-ERROR.
    FIND Estado WHERE Estado.Id-Estado = Almacen.Id-Estado NO-LOCK NO-ERROR.
    FIND Pais WHERE Pais.Id-Pais = Almacen.Id-Pais NO-LOCK NO-ERROR.
END.

ASSIGN
    l-TipoEntrega = Entrega.Descr
    l-CalleEmb    = (IF (Pedido.Id-Entrega = 3 OR Pedido.Id-Entrega = 16)
                     THEN TRIM(Almacen.CalleNo)
                     ELSE TRIM(Pedido.CalleNo1))
    l-ColEmb      = (IF (Pedido.Id-Entrega = 3 OR Pedido.Id-Entrega = 16)
                     THEN TRIM(Almacen.Colonia)
                     ELSE TRIM(Pedido.Colonia1))
    l-CdEmb       = (IF (Pedido.Id-Entrega = 3 OR Pedido.Id-Entrega = 16)
                     THEN TRIM(Ciudad.Nombre) + " " + TRIM(Estado.Nombre) + " " + TRIM(Pais.Nombre) + " " + TRIM(STRING(Almacen.CP))
                     ELSE TRIM(TRIM(Pedido.Ciudad1)) + (IF Pedido.Ciudad1 = "" THEN "" ELSE ', ') + TRIM(TRIM(Pedido.Estado1)) + " " + STRING(Pedido.CP1))
    l-DirEmb      = IF l-CalleEmb <> ""
                    THEN TRIM(l-CalleEmb) + (IF l-CalleEmb = "" THEN "" ELSE ', ') + TRIM(l-ColEmb)
                    ELSE "MISMA DIRECCI�N DE FACTURACI�N".
    
    
DISPLAY STREAM s-Salida
    FILL("=",28) FORMAT "x(28)"
    "DATOS DE FACTURACI�N"
    FILL("=",29) FORMAT "x(29)"
    FILL("=",30) FORMAT "x(30)" AT 81
    "DATOS DE EMBARQUE"
    FILL("=",30) FORMAT "x(30)"
    SKIP
    "Cliente        :" Pedido.Id-Cliente Pedido.RazonSocial FORMAT 'x(55)'
    'Tipo de entrega:' AT 81 l-TipoEntrega
    SKIP
    "RFC            :"
    Pedido.RFC
    'Atenci�n a     :' AT 81 Pedido.RazonSocial1
    SKIP
    'Direcci�n      :' TRIM(STRING(Pedido.CalleNo,'X(60)')) + ' ' + TRIM(STRING(Pedido.Colonia,'x(60)')) FORMAT 'x(60)'
    'Direcci�n      :' AT 81 l-DirEmb FORMAT "X(60)"
    SKIP
    'Ciudad y estado:' TRIM(l-Ciudad) + ' ' + TRIM(l-Estado)  + ' ' + Pedido.CP FORMAT 'x(60)'
    'Ciudad y estado:' AT 81 TRIM(l-CdEmb) FORMAT 'x(60)'
    SKIP
    'Delegaci�n     :' l-Delegacion
    'Delegaci�n     :' AT 81 Pedido.Delegacion1
    SKIP
    'Condici�n Vta. :' CondVta.Descr
    SKIP
    FILL("=",79) FORMAT "x(79)"
    FILL("=",79) FORMAT "x(79)" AT 81
WITH FRAME f-Enca OVERLAY NO-LABEL CENTERED WIDTH 162 NO-BOX.

FOR EACH DetPedido WHERE DetPedido.Id-Pedido = Pedido.Id-Pedido
                     AND DetPedido.Resto = Pedido.Resto NO-LOCK BY DetPedido.Reng:
    FIND ArtPres WHERE ArtPres.Id-Articulo = DetPedido.Id-Articulo
                   AND ArtPres.Id-Pres = DetPedido.Id-Pres NO-LOCK NO-ERROR.
    FIND Kolor OF DetPedido NO-LOCK NO-ERROR.

    DISPLAY STREAM s-Salida
        DetPedido.Id-Articulo
        DetPedido.Descr
        Kolor.Abrev       WHEN AVAILABLE Kolor
        DetPedido.CantPed COLUMN-LABEL 'Cantidad!Pedida' WHEN DetPedido.CantPed <> 0
        ArtPres.Descr     COLUMN-LABEL 'Present' WHEN AVAILABLE ArtPres
        /*
        DetPedido.Importe WHEN DetPedido.Importe <> 0
        DetPedido.IVA     WHEN DetPedido.IVA <> 0
        (DetPedido.Importe + DetPedido.IVA) (TOTAL) WHEN DetPedido.Importe <> 0 COLUMN-LABEL "Total"
        */
    WITH FRAME f-Imprime DOWN WIDTH 162.
    DOWN STREAM s-Salida WITH FRAME f-Imprime.
END.

OUTPUT STREAM s-Salida CLOSE.

/*
IF {/usr2/adosa/includes/salt0005.i} = 'Saltillo' THEN
    UNIX SILENT VALUE("rcp " + l-Archivo + " ado9:/usr3/tmp/").
*/

DO ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE:
    ASSIGN
        l-contenido = l-contenido +
                      "<br/><br/>No olvides visitar nuestra p�gina de Internet " +
                      '<a href="http://www.adosa.com.mx">www.adosa.com.mx</a>'.

    ASSIGN l-eMail = ''.

    IF Pedido.Compra <> '' THEN DO:
        FIND FIRST CteEmp WHERE CteEmp.Id-Cliente = Pedido.Id-cliente
                            AND CteEmp.Nombre BEGINS Pedido.Compra
                            AND CteEmp.Nombre <> '' NO-LOCK NO-ERROR.
        IF AVAILABLE CteEmp AND CteEmp.e-Mail <> '' THEN
            ASSIGN l-eMail = CteEmp.e-Mail.
        ELSE
            ASSIGN l-eMail = Pedido.e-Mail.
    END.
    ELSE
        ASSIGN l-eMail = Pedido.e-Mail.
        
    IF l-eMail = "" THEN
        ASSIGN l-eMail = Pedido.BuzonFiscal.

    DO WHILE INDEX(l-eMail,' ') <> 0:
        SUBSTRING(l-eMail,INDEX(l-eMail,' '),1) = ''.
    END.
    
    IF {sist0001.i} = "DESARROLLO" THEN  
        l-eMail = "desarrollo10@adosa.com.mx".

    IF l-eMail <> '' THEN DO:
        RUN /usr2/adosa/procs/rptpdf01.p (INPUT l-Archivo,
                                          INPUT "H",
                                          OUTPUT l-arch2).
        
        DO l-i = 1 TO 100:
            IF SEARCH("/usr2/compartido/" + STRING(l-i) + SUBSTRING(TRIM(l-arch2),R-INDEX(TRIM(l-Arch2),"/") + 1,LENGTH(TRIM(l-arch2)))) = ? THEN LEAVE. 
        END.
         
        UNIX SILENT VALUE('cp ' + TRIM(l-Arch2) + ' /usr2/compartido/' + STRING(l-i) + SUBSTRING(TRIM(l-arch2),R-INDEX(TRIM(l-Arch2),"/") + 1,LENGTH(TRIM(l-arch2)))).
         
        ASSIGN
            v-arch1 = SUBSTRING(TRIM(l-arch2),R-INDEX(TRIM(l-Arch2),'/') + 1, R-INDEX(TRIM(l-arch2),".")) + ".pdf:type=application/pdf:filetype=B64ENCODED".
        
        IF v-arch1 MATCHES "*.txt*" THEN
            v-arch1 = REPLACE(v-arch1,".txt","").
        
        ASSIGN
            l-arch2 = '/usr2/compartido/' + STRING(l-i) + SUBSTRING(TRIM(l-arch2),R-INDEX(TRIM(l-Arch2),'/') + 1,LENGTH(TRIM(l-arch2))).
            
        IF AVAILABLE Usuario THEN 
            ASSIGN
                v-mailde     = Usuario.e-mail + ";" + Usuario.Nom-Usuario
                v-respondera = Usuario.e-mail + ";" + Usuario.Nom-Usuario.
        ELSE 
            ASSIGN
                v-mailde     = "zcorreo@adosa.com.mx;Correo autom�tico ADOSA"
                v-respondera = "zcorreo@adosa.com.mx;Correo autom�tico ADOSA".
        
        /* Asigna el mail de respuesta */
        IF v-respondera <> "" THEN
            ASSIGN
                v-mailde = v-mailde + "^" + v-respondera.
                
        ASSIGN
            v-mailde = v-mailde + CHR(1) + "No,No".

        RUN /usr2/adosa/procs/correo01.p (INPUT l-eMail,
                                          INPUT v-mailde,
                                          INPUT "",
                                          INPUT v-arch1, /*nombre del archivo solamente */
                                          INPUT l-arch2, /*ruta completa del archivo */
                                          INPUT l-Asunto,
                                          INPUT l-Contenido,
                                          OUTPUT v-enviado).
        /**/
        IF TODAY <= 04/30/2016 THEN DO:                       
            RUN /usr2/adosa/procs/correo01.p (INPUT "aayala@adosa.com.mx",
                                              INPUT v-mailde,
                                              INPUT "",
                                              INPUT v-arch1, /*nombre del archivo solamente */
                                              INPUT l-arch2, /*ruta completa del archivo */
                                              INPUT l-Asunto,
                                              INPUT l-Contenido,
                                              OUTPUT v-enviado).
        END.
        /**/
    END.
END.
