@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : ReporteCartera.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis6
    Created     : Fri May 09 09:52:50 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE l-num         AS INTEGER.  
DEFINE VARIABLE l-tot-total   AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-vig     AS DECIMAL   FORMAT ">>>,>>>,>>9.99". 
DEFINE VARIABLE l-tot-ven     AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-porv    AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-clase       AS CHARACTER FORMAT "X(20)". 
DEFINE VARIABLE l-segmento    AS CHARACTER FORMAT "X(20)".
DEFINE VARIABLE l-estatus     AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE l-resp        AS CHARACTER FORMAT "X(30)" .
DEFINE VARIABLE l-moneda      AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE l-tipo-moneda AS INTEGER.
DEFINE VARIABLE l-prompago    AS DECIMAL.
DEFINE VARIABLE l-tot-30      AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-31      AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-61      AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-91      AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-dia         AS INTEGER.
DEFINE VARIABLE l-dia-max     AS INTEGER  FORMAT "zz9" NO-UNDO.
DEFINE VARIABLE l-saldo       AS DECIMAL  FORMAT ">>>,>>>,>>9.99".


    
DEFINE TEMP-TABLE ttCartera
    FIELD id           AS INTEGER
    FIELD clasecliente AS CHARACTER FORMAT "X(12)"
    FIELD numcliente   AS INTEGER
    FIELD cliente      AS CHARACTER FORMAT "X(40)"
    FIELD segmento     AS CHARACTER FORMAT "X(12)"
    FIELD tipoMoneda   AS CHARACTER FORMAT "X(15)"
    FIELD saldo        AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD montovencido AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD vigente      AS DECIMAL   FORMAT ">>>,>>>,>>9.99" 
    FIELD porvencer    AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD treinta      AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD sesenta      AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD noventa      AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD noventamas   AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD lineacredito AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD diacartera   AS INTEGER
    FIELD promedio     AS DECIMAL
    FIELD plazo        AS INTEGER
    FIELD responsable  AS CHARACTER FORMAT "X(40)"
    FIELD Fecha        AS DATE
    INDEX idx-clase id ASCENDING numcliente ASCENDING cliente ASCENDING.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetReporteCartera:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  pClaseCte AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER  pTipo     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER pMoneda AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER pZona AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER pSegmento AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pCalidad AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER pFecha AS CHARACTER NO-UNDO.      
    DEFINE OUTPUT PARAMETER TABLE FOR ttCartera.   
    
    DEFINE VARIABLE cFechaISO      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dFecha         AS DATE     NO-UNDO.
    
    /* Extraer solo la parte de fecha (primeros 10 caracteres) */
cFechaISO = SUBSTRING(pFecha, 1, 10).  /* Resultado: "2025-01-15" */

/* Reorganizar a un formato que DATE() entienda, por ejemplo "01/15/2025" */
cFechaISO = SUBSTRING(cFechaISO, 9, 2) + "/" +  /* DD */ 
            SUBSTRING(cFechaISO, 6, 2) + "/" +  /* MM */            
            SUBSTRING(cFechaISO, 1, 4).         /* YYYY */

/* Convertir a tipo DATE */     
dFecha = DATE(cFechaISO).
    
    ASSIGN 
        l-num    = 0
        l-moneda = "".
        
    //IF pClaseCte = ? THEN pClaseCte = 0. 
    IF pTipo     = ? THEN pTipo = 0.
    IF pSegmento = "0" THEN pSegmento = "".  
    EMPTY TEMP-TABLE ttCartera.
    FOR EACH Cliente WHERE (IF pClaseCte <> 0 AND pClaseCte <> ? THEN Cliente.Id-ClaseCte = pClaseCte ELSE TRUE)
                       AND (IF pZona <> 0 AND pZona <> ? THEN Cliente.Id-Zona = pZona ELSE TRUE)
                       AND (IF pSegmento <> "" AND pSegmento <> ? THEN Cliente.Id-Segmento = pSegmento ELSE TRUE)
                       AND (IF pCalidad <> 0 AND pCalidad <> ? THEN Cliente.Id-Calidad = pCalidad ELSE TRUE) NO-LOCK:
        
        FOR EACH Movcliente WHERE Movcliente.Id-Cliente = Cliente.Id-Cliente 
                              AND (IF pFecha = ? THEN Movcliente.FecReg <= TODAY ELSE Movcliente.FecReg = dFecha)
                              AND MovCliente.Id-MC  <= 3                     
                              AND MovCliente.Afectado  
                              AND (IF pMoneda <> 0 AND pMoneda <> ? THEN Movcliente.Id-Moneda = pMoneda ELSE TRUE)                   
                             NO-LOCK  BREAK  BY Cliente.Id-Cliente
                                             BY Cliente.RazonSocial 
                                             BY MovCliente.Id-Cliente:
            IF MovCliente.Saldo <= 0 THEN NEXT.                    
            
            ASSIGN
                l-clase    = "Local" 
                l-segmento = ""
                l-resp     = "". 
            
            FIND FIRST ClaseCte WHERE ClaseCte.id-ClaseCte = Cliente.Id-ClaseCte NO-LOCK   NO-ERROR.
            IF AVAILABLE ClaseCte THEN l-clase = ClaseCte.Descr. 
            
            FIND FIRST SegmentoCte WHERE SegmentoCte.id-SegmentoCte = Cliente.Id-SegmentoCte NO-LOCK NO-ERROR.
            IF AVAILABLE SegmentoCte THEN l-segmento = SegmentoCte.Descr.
            
            FIND FIRST Resp WHERE Resp.Id-Resp = Cliente.Id-Resp NO-LOCK NO-ERROR.
            IF AVAILABLE Resp THEN l-resp = Resp.Nombre.   
        
            FIND FIRST Factura WHERE Factura.Id-Factura = MovCliente.RefSaldo
                AND Factura.Id-Cliente = Cliente.Id-Cliente
                AND Factura.FecReg     = MovCliente.FecReg
                NO-LOCK NO-ERROR.
            IF AVAILABLE Factura THEN l-tipo-moneda = Factura.Id-Moneda.
         
            FIND FIRST Moneda WHERE moneda.id-moneda = l-tipo-moneda NO-LOCK NO-ERROR.
            IF AVAILABLE moneda THEN l-moneda = moneda.nombre.
            
            IF FIRST-OF(Cliente.Id-Cliente) THEN  
                ASSIGN
                    l-tot-total = 0
                    l-tot-vig   = 0
                    l-tot-ven   = 0
                    l-tot-porv  = 0
                    l-estatus   = ""
                    l-saldo     = 0
                    l-tot-30    = 0
                    l-tot-31    = 0
                    l-tot-61    = 0
                    l-tot-91    = 0. 
            l-saldo = l-saldo + MovCliente.Saldo.   
            
            IF MovCliente.fecven <  TODAY THEN 
            DO:
                ASSIGN
                    l-tot-ven = l-tot-ven  +  Movcliente.Saldo
                    l-dia =  TODAY - MovCliente.FecVenc .   
        
                IF l-dia <= 30 THEN
                   ASSIGN l-tot-30 = l-tot-30 + Movcliente.Saldo. /* 1-30 */
                ELSE IF l-dia <= 60 THEN
                   ASSIGN l-tot-31 = l-tot-31 + Movcliente.Saldo. /* 31-60 */
                ELSE IF l-dia <= 90 THEN
                   ASSIGN l-tot-61 = l-tot-61 + Movcliente.Saldo. /* 61-90 */
                ELSE  
                   ASSIGN l-tot-91 = l-tot-91 + Movcliente.Saldo. /* 91.. + */     
            END.  
            IF MovCliente.fecven >= TODAY + 16 THEN 
            DO:
                ASSIGN 
                    l-tot-vig = l-tot-vig +  Movcliente.Saldo.
            END.
            IF MovCliente.fecven >= TODAY AND
                MovCliente.fecven <= TODAY + 15 THEN 
            DO:
                ASSIGN 
                    l-tot-porv = l-tot-porv +  Movcliente.Saldo.
            END.  
            
                    
              
            IF LAST-OF(Cliente.id-cliente) THEN   
            DO: 
               
                l-num = l-num + 1.  
                  
                FIND FIRST ttCartera WHERE ttCartera.id = l-num NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ttCartera THEN  
                    CREATE ttCartera.
                ASSIGN
                    ttCartera.id           = l-num
                    ttCartera.clasecliente = l-clase
                    ttCartera.numcliente   = Cliente.id-cliente
                    ttCartera.cliente      = Cliente.RazonSocial
                    ttCartera.segmento     = l-segmento
                    ttCartera.tipoMoneda   = l-moneda
                    ttCartera.saldo        = l-saldo 
                    ttCartera.montovencido = l-tot-ven
                    ttCartera.vigente      = l-tot-vig
                    ttCartera.porvencer    = l-tot-porv
                    ttCartera.treinta      = l-tot-30
                    ttCartera.sesenta      = l-tot-31
                    ttCartera.noventa      = l-tot-61
                    ttCartera.noventamas   = l-tot-91
                    ttCartera.lineacredito = Cliente.Limite
                    ttCartera.diacartera   = l-dia-max
                    ttCartera.promedio     = l-prompago
                    ttCartera.plazo        = Cliente.Plazo
                    ttCartera.responsable  = l-resp 
                    ttCartera.Fecha        = MovCliente.FecReg.  
                RELEASE ttCartera.
                ASSIGN
                l-saldo    = 0
                l-tot-vig  = 0
                l-tot-ven  = 0
                l-tot-porv = 0
                l-tot-30   = 0
                l-tot-31   = 0
                l-tot-61   = 0
                l-tot-91   = 0.
            END.
        END.
    END. 
    
    IF pTipo = 1 THEN DO:
        /* vigente */ 
       FOR EACH ttCartera WHERE ttCartera.vigente <= 0 :
           DELETE ttCartera.
       END.     
    END.
    ELSE IF pTipo = 2 THEN DO:
        /* por vencer */ 
       FOR EACH ttCartera WHERE ttCartera.porvencer <= 0 :
           DELETE ttCartera.
       END.      
    END.
    ELSE IF pTipo = 3 THEN DO :
         /* vencido */ 
       FOR EACH ttCartera WHERE ttCartera.montovencido <= 0 :
           DELETE ttCartera.
       END.     
    END.
    
    FOR EACH ttCartera  
        WHERE (pclasecte <> 0)   /* Caso original: clase diferente de 0 */
           OR (pclasecte = 0 AND pTipo <> 0): 
      DO TRANSACTION:                                                                             
        RUN cxcb0270.p(INPUT ttCartera.numcliente,INPUT TODAY,OUTPUT l-dia-max ,OUTPUT l-prompago).
         IF l-dia-max = ? THEN l-dia-max = 0.
      END.
      ASSIGN  ttCartera.diacartera   = l-dia-max
              ttCartera.promedio     = INTEGER(ROUND(l-prompago, 0)). /* Redondea a un entero */ 
    END. 

END PROCEDURE.

