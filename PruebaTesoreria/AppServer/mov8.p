DEFINE VARIABLE l-num                 AS INTEGER.
DEFINE VARIABLE v-resp                AS CHARACTER.
DEFINE VARIABLE v-clase               AS CHARACTER.
DEFINE VARIABLE v-dia                 AS INTEGER.
DEFINE VARIABLE v-mes                 AS INTEGER.
DEFINE VARIABLE i-mes                 AS INTEGER.
DEFINE VARIABLE i-anio                AS INTEGER.
DEFINE VARIABLE i-clase               AS INTEGER.
DEFINE VARIABLE l-lista               AS CHARACTER.
DEFINE VARIABLE v-tipo-cte            AS CHARACTER.
DEFINE VARIABLE v-documento           AS CHARACTER FORMAT "X(30)" .

DEFINE TEMP-TABLE ttDeposito NO-UNDO
    FIELD NumList               AS INTEGER
    FIELD Clase                 AS CHARACTER FORMAT "X(30)" 
    FIELD Zona                  AS CHARACTER FORMAT "X(30)"
    FIELD TipoDeCliente         AS CHARACTER FORMAT "X(30)"
    FIELD NumCliente            AS INTEGER
    FIELD Cliente               AS CHARACTER FORMAT "X(30)"
    FIELD FechaDeposito         AS DATE      FORMAT 99/99/9999
    FIELD Hora                  AS CHARACTER FORMAT "X(6)"
    FIELD Banco                 AS CHARACTER FORMAT "X(30)"
    FIELD Descripcion           AS CHARACTER FORMAT "X(30)"
    FIELD Importe               AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD FechaAplicacion       AS DATE      FORMAT 99/99/9999
    FIELD Documento             AS CHARACTER FORMAT "X(30)"
    FIELD Anticipo              AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD Aplicado              AS LOGICAL   
    FIELD Responsable           AS CHARACTER FORMAT "X(30)"
    INDEX idx-respo NumCliente ASCENDING.
DEFINE DATASET dsDeposito FOR ttDeposito.



/*UPDATE      */
/*    i-mes   */
/*    i-anio  */
/*    i-clase.*/ 

 
IF i-mes  = 0 THEN i-mes   = MONTH(TODAY) .
IF i-anio = 0 THEN i-anio  = YEAR(TODAY). 

ASSIGN 
    /* Excluir ctes del 1-11 excepto al 3 en depositos credido-contado */
    l-lista = "1,2,4,5,6,7,8,9,10,11"
    l-num   = 0
    i-clase = 1.

EMPTY TEMP-TABLE ttDeposito.     
/*OUTPUT TO "/Users/sis10/Progress/Developer Studio 12.8/workspace/RestADOSACyc/hola.txt".*/
OUTPUT to /home/sis10/deposito-manuel-2.txt.
FOR EACH Depbanco WHERE MONTH (Depbanco.FecDep) = i-mes 
                    AND YEAR(Depbanco.FecDep)   = i-anio NO-LOCK :                   
    FOR EACH Cliente WHERE Cliente.Id-Cliente = Depbanco.Id-Cliente 
                       AND Cliente.Id-clasecte = i-clase
        NO-LOCK BREAK BY Depbanco.Id-Banco 
                      BY DepBanco.Id-Cliente 
                      BY Cliente.Id-Cliente :
        IF LOOKUP(STRING (Cliente.Id-Cliente), SUBSTITUTE(l-lista,",",""))> 0 THEN NEXT.        
        FIND FIRST ClaseCte WHERE ClaseCte.id-ClaseCte = i-clase  NO-LOCK NO-ERROR.    
        FIND FIRST Resp     WHERE Resp.Id-Resp = Cliente.Id-Resp     NO-LOCK NO-ERROR.
        FIND FIRST Banco    WHERE Banco.Id-Banco = Depbanco.Id-Banco NO-LOCK NO-ERROR.
        
        ASSIGN v-documento = " ".
        FOR EACH DocAcuse WHERE DocAcuse.Id-Acuse = DepBanco.Id-Acuse NO-LOCK :
            ASSIGN v-documento = DocAcuse.Documento. 
            IF DocAcuse.Sec >= 2 THEN  ASSIGN v-documento = v-documento + "|" + DocAcuse.Documento . 
        END.
         
     
        ASSIGN        
        v-tipo-cte = " "
        l-num = l-num + 1
        . 
        /* Revisar tipo contado tipocte = 4 */
        IF DepBanco.TipoCte = 4 THEN 
        DO:
          ASSIGN v-tipo-cte = "Contado".
        END.
        ELSE DO:
          ASSIGN v-tipo-cte = "Credito" .
        END.
       
/*        MESSAGE l-num view-as alert-box.*/
        
        
/*        FIND FIRST ttDeposito WHERE ttDeposito.NumList = l-num NO-LOCK NO-ERROR.*/
/*        IF NOT AVAILABLE ttDeposito THEN                                        */
/*        CREATE ttDeposito.                                                      */
/*        ASSIGN                                                                  */
/*               ttDeposito.NumList         = l-num                               */
/*               ttDeposito.Clase           = ClaseCte.Descr                      */
/*               ttDeposito.Zona            = "Zona"                              */
/*               ttDeposito.TipoDeCliente   = v-tipo-cte                          */
/*               ttDeposito.NumCliente      = Cliente.Id-Cliente                  */
/*               ttDeposito.Cliente         = Cliente.RazonSocial                 */
/*               ttDeposito.FechaDeposito   = Depbanco.FecDep                     */
/*               ttDeposito.Hora            = Depbanco.HoraDep                    */
/*               ttDeposito.Banco           = Banco.Nombre                        */
/*               ttDeposito.Descripcion     = Depbanco.Descripcion                */
/*               ttDeposito.Importe         = Depbanco.Importe                    */
/*               ttDeposito.FechaAplicacion = Depbanco.FecAplica                  */
/*               ttDeposito.Documento       = "0000"                              */
/*               ttDeposito.Anticipo        = 0                                   */
/*               ttDeposito.Aplicado        = Depbanco.Conciliado                 */
/*               ttDeposito.Responsable     = Resp.Nombre .                       */
/*               RELEASE ttDeposito.                                              */
          
        DISPLAY
            l-num
            DepBanco.Id-Acuse
            DepBanco.Id-AcuseAnt
            Clasecte.descr FORMAT "x(12)"
            "zona"
            v-tipo-cte
            DepBanco.TipoCte
            Cliente.Id-Cliente
            Cliente.RazonSocial FORMAT "X(50)"
            Depbanco.FecDep
            Depbanco.HoraDep
            Depbanco.Id-Banco
            Depbanco.Sucursal
            Banco.Nombre
            Depbanco.Descripcion
            Depbanco.Importe
            Depbanco.FecAplica
            v-documento
            DocAcuse.Documento WHEN  AVAILABLE DocAcuse
            "document5551"
            "003"
            Depbanco.Conciliado FORMAT "SI/NO"
            Resp.Nombre
            WITH FRAME a
            DOWN WIDTH 400.
    END. 
END.
                          
/*   FOR EACH ttDeposito: */
/*      DISPLAY ttDeposito*/
/*      WITH FRAME a      */
/*      DOWN WIDTH 400.   */
/*   END.                 */
OUTPUT close.


