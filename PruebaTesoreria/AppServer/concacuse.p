@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*
  Programa : concacuse.p
  Funcion  : POST para conciliacion de cobranza (generacion de acuses)
  Autor    : FLC
  Fecha    : 7 DIC 2024
*/

DEF TEMP-TABLE ttDocto                      
    FIELD IdUser      AS CHAR
    FIELD IdCliente   LIKE Acuse.Id-Cliente
    FIELD IdDoc       LIKE DocAcuse.Documento
    FIELD FecReg      LIKE Factura.FecReg
    FIELD RazonSocial LIKE Factura.RazonSocial
    FIELD Total       AS DECIMAL
    FIELD SubTotal    LIKE Factura.SubTotal
    FIELD TDoc        AS CHAR
    FIELD ImpPago     AS DECIMAL
    FIELD ImpAnt      AS DECIMAL
    FIELD Desc1       AS DECIMAL
    FIELD Desc2       AS DECIMAL           
    FIELD Desc3       AS DECIMAL
    FIELD Desc4       AS DECIMAL
    FIELD Desc5       AS DECIMAL
    FIELD Desc6       AS DECIMAL    
    FIELD Desc7       AS DECIMAL
    FIELD Desc8       AS DECIMAL
    FIELD Desc9       AS DECIMAL
    FIELD Desc10      AS DECIMAL
    FIELD Desc11      AS DECIMAL
    FIELD Desc12      AS DECIMAL   
    INDEX Idx-Def TDoc IdDoc.
    
DEF TEMP-TABLE ttAnticipos
    FIELD IdAnticipo  LIKE Anticipo.Id-Anticipo
    FIELD ImpAplicado LIKE DetAnticipo.Importe
    FIELD IdDoc       LIKE DocAcuse.Documento
    FIELD RelationId  AS CHAR /* Relación con ttDocto */
    INDEX Idx-Def IdDoc.
    
DEF TEMP-TABLE ttPago     
    FIELD FormaPago     AS INTEGER 
    FIELD Importe       AS DECIMAL
    FIELD Rec           AS RECID
    FIELD FecDep        AS DATE
    FIELD IdBanco       AS INT
    FIELD Cuenta        AS CHAR  
    FIELD FolioCheque   AS CHAR
    FIELD FechaCheque   AS DATE 
    FIELD Observaciones AS CHAR
    FIELD RelationId    AS CHAR /* Relación con ttDocto */
    INDEX Idx-Def Importe DESCENDING.  

DEFINE DATASET dsConciliacion FOR 
    ttDocto,
    ttAnticipos,
    ttPago
    DATA-RELATION drDoctoAnticipos FOR ttDocto, ttAnticipos
    RELATION-FIELDS (IdDoc, IdDoc)
    NESTED
    DATA-RELATION drDoctoPago FOR ttDocto, ttPago
    RELATION-FIELDS (IdDoc, RelationId)
    NESTED.

DEFINE TEMP-TABLE ttConc NO-UNDO
    FIELD Documento   AS CHARACTER
    FIELD Descripcion AS CHARACTER
    FIELD Vencimiento AS DATE
    FIELD Saldo       AS DECIMAL
    FIELD TipoMoneda  AS CHARACTER
    FIELD Dias        AS INT.
    
/* **********************  Internal Procedures  *********************** */

/* Procedimiento REST */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetFacturas:
    DEFINE INPUT PARAMETER pCliente AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttConc.

    DEF VAR l-saldo LIKE MovCliente.Saldo NO-UNDO.
    DEF VAR l-dias  AS INTEGER NO-UNDO.
    DEF VAR i       AS INTEGER.

    DEF BUFFER MovCliente_bf FOR MovCliente.
    /* Limpiar la tabla temporal */
    EMPTY TEMP-TABLE ttConc.

    /* Buscar movimientos pendientes (deudas) */
    FOR EACH MovCliente WHERE
        MovCliente.Id-MC <= 3 AND
        MovCliente.Saldo > 0  AND Movcliente.id-cliente = pCliente NO-LOCK
        USE-INDEX idx-mov :
         

        ASSIGN 
            l-saldo = MovCliente.Saldo.
        FOR EACH MovCliente_bf
            WHERE MovCliente_bf.RefSaldo = MovCliente.Refsaldo AND
            MovCliente_bf.Afectado  = FALSE NO-LOCK :
            ASSIGN 
                l-Saldo = l-Saldo + MovCliente_bf.importe.
        END.
        FOR EACH DocAcuse WHERE DocAcuse.Documento = MovCliente.Documento NO-LOCK :
            ASSIGN 
                l-saldo = l-saldo - DocAcuse.ImpPago -
                   DocAcuse.ImpDevol - DocAcuse.ImpDescPP
                  - DocAcuse.ImpDescAdc - DocAcuse.ImpDescEsp.
        END.
         
        FIND TabMC OF MovCliente NO-LOCK NO-ERROR.
        IF l-saldo > 0 THEN 
        DO:
            /* Crear un registro en la tabla temporal */
            CREATE ttConc.
            ASSIGN
                ttConc.Documento   = MovCliente.RefSaldo
                ttConc.Descripcion = IF AVAILABLE TabMc THEN TabMC.Descr ELSE ""
                ttConc.Vencimiento = MovCliente.FecVen
                ttConc.Saldo       = l-saldo
                ttConc.Dias        = TODAY - MovCliente.FecVenc. 
        END.   
    END.
END PROCEDURE.


@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostGeneraAcuse:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER DATASET FOR dsConciliacion.
    DEF OUTPUT PARAMETER p-Acuse AS CHAR NO-UNDO.
    

    DEF    VAR ip-formapago    AS CHAR      NO-UNDO.
    DEF    VAR l-Veces         AS INTEGER   NO-UNDO.
    DEF    VAR l-NPagos        AS INTEGER   NO-UNDO.
    DEF    VAR l-Nda           AS INTEGER   NO-UNDO.
    DEF    VAR l-TP            AS INTEGER   NO-UNDO.
    DEF    VAR l-recmov        AS RECID     NO-UNDO.
    DEF    VAR l-FecVence      AS DATE      NO-UNDO.
    DEF    VAR l-Ubic          LIKE MovCliente.Id-Ubic NO-UNDO.

    DEF    VAR l-folAcuse      LIKE Acuse.Id-Acuse NO-UNDO INITIAL "".
    DEF    VAR l-folAntAcuse   LIKE Acuse.Id-Acuse NO-UNDO INITIAL "".
    DEF    VAR l-ImpPagado     AS DECIMAL   NO-UNDO.
    DEF    VAR l-ImpDeposito   AS DECIMAL   NO-UNDO.
    DEF    VAR l-depsantander  AS DECIMAL   NO-UNDO.
    DEF    VAR l-RestoAnt      AS DECIMAL   NO-UNDO.
    DEF    VAR l-AntApl        AS DECIMAL   NO-UNDO.
    DEF    VAR l-FecDep        AS DATE      NO-UNDO.
    DEF    VAR l-Comen2        AS CHAR      NO-UNDO.
    DEF    VAR l-Comen3        AS CHAR      NO-UNDO.
    DEF    VAR l-UsuApl        AS CHAR      NO-UNDO INITIAL "".
    DEF    VAR meses           AS CHARACTER EXTENT 12 NO-UNDO INITIAL 
        ["ENE","FEB","MAR","ABR","MAY","JUN","JUL","AGO","SEP","OCT","NOV","DIC"].
        
    DEFINE VAR l-ValorOld      LIKE CambioCte.ValorOld.
    DEFINE VAR l-ValorNuevo    LIKE CambioCte.ValorNuevo.
    DEFINE VAR l-descsantander AS CHAR      NO-UNDO INITIAL "".
    DEFINE VAR l-appsantander  AS LOGICAL   NO-UNDO INITIAL FALSE.
   

    /* VALIDACION PARA REVISAR SI YA SE CONCILIO UN PAGO SANTANDER */
    l-FecDep = TODAY.
    l-Comen2 = "".
    l-depsantander = 0.
    l-descsantander = "".
    l-appsantander = FALSE.
    l-TP = 0.
    FIND FIRST ttPago WHERE ttPago.Rec <> 0 NO-LOCK NO-ERROR.  // Los de Santander en Rec envia distinto a 0   
    IF AVAILABLE ttPago THEN 
    DO:
        FIND DepBanco WHERE RECID(DepBanco) = ttPago.Rec EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF NOT AVAILABLE DepBanco THEN
        DO:
            ASSIGN 
                p-Acuse = "Deposito en uso por otro usuario".    
            RETURN.
        END.   
        IF AVAILABLE DepBanco AND DepBanco.Conciliado = TRUE THEN
        DO:
            ASSIGN 
                p-Acuse = DepBanco.Id-Acuse.              
            RETURN.
        END.   
        IF AVAILABLE DepBanco THEN
        DO:
            ASSIGN 
                l-FecDep        = DepBanco.FecDep
                l-Comen2        = STRING(DAY(DepBanco.FecDep)) + '/' +
                        STRING(meses[MONTH(DepBanco.FecDep)]) + '/' +
                        STRING(YEAR(DepBanco.FecDep)) + ' ' +
                        IF LENGTH(DepBanco.HoraDep) = 4 THEN SUBSTRING(STRING(DepBanco.HoraDep),1,2) + ':' + 
                           SUBSTRING(STRING(DepBanco.HoraDep),3,2)
                       ELSE '0' + SUBSTRING(STRING(DepBanco.HoraDep),1,1) + ':' + SUBSTRING(STRING(DepBanco.HoraDep),2,2)
                l-depsantander  = DepBanco.Importe
                l-descsantander = DepBanco.Descripcion
                l-appsantander  = TRUE.
        END.
    END.
    ELSE 
    DO:
        FIND FIRST ttPago WHERE ttPago.FecDep <> ? NO-LOCK NO-ERROR. 
        IF AVAILABLE ttPago THEN 
        DO:
            ASSIGN
                l-FecDep = ttPago.FecDep
                l-TP     = ttPago.FormaPago. 
        END.
    END.
    
    /*                   
    FIND Folio WHERE Folio.Id-Doc = "ACUSE" AND Folio.Id-Alm = "N" 
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE Folio THEN 
    DO:
        CREATE Folio.
        ASSIGN 
            Folio.Id-Doc  = "ACUSE"
            Folio.Id-Alm  = "N"
            Folio.Prefijo = "N"
            Folio.Folio   = 1.
    END.      

    ASSIGN 
        l-folacuse  = STRING(Folio.Folio,"9999999") + TRIM(Folio.PreFijo)
        l-folacuse  = SUBSTRING(l-folacuse,LENGTH(l-folacuse) - 6,7)
        Folio.Folio = Folio.Folio + 1.
    */
    
    /* --- Búsqueda inicial sin bloquear --- */
    FIND Folio WHERE Folio.Id-Doc = "ACUSE" AND Folio.Id-Alm = "N" NO-LOCK NO-ERROR.

    /* --- Crear registro si no existe (con transacción) --- */
    IF NOT AVAILABLE Folio THEN 
    DO:
        DO TRANSACTION:
            FIND Folio WHERE Folio.Id-Doc = "ACUSE" AND Folio.Id-Alm = "N" 
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE Folio THEN 
            DO:
                CREATE Folio.
                ASSIGN 
                    Folio.Id-Doc  = "ACUSE"
                    Folio.Id-Alm  = "N"
                    Folio.Prefijo = "N"
                    Folio.Folio   = 1.
            END.
        END.
    END.

    /* --- Generar folio e incrementar (con bloqueo mínimo) --- */
    DO TRANSACTION:
        FIND Folio WHERE Folio.Id-Doc = "ACUSE" AND Folio.Id-Alm = "N" 
                   EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF NOT AVAILABLE Folio THEN
        DO:
            ASSIGN   
                p-Acuse = "fDeposito en uso por otro usuario".    
            RETURN.
        END.
        IF AVAILABLE Folio THEN 
        DO:   
            ASSIGN 
            l-folacuse  = STRING(Folio.Folio, "9999999") + TRIM(Folio.PreFijo)
            l-folacuse  = SUBSTRING(l-folacuse, LENGTH(l-folacuse) - 6, 7)
            Folio.Folio = Folio.Folio + 1.
            RELEASE Folio.
        END.  
    END.       
    
    FIND FIRST ttDocto WHERE ttDocto.IdCliente > 0 NO-LOCK NO-ERROR.
    IF AVAILABLE ttDocto THEN l-UsuApl = ttDocto.IdUser.
    
    MESSAGE "Procesando concacuse.p del Usuario: " + STRING(l-UsuApl) VIEW-AS ALERT-BOX.    

    /* Cuando envien observaciones que va en los comentarios */ 
    FIND FIRST ttPago WHERE ttPago.Observaciones <> "" NO-LOCK NO-ERROR.
    IF AVAILABLE ttPago THEN l-Comen2 = ttPago.Observaciones.
    
    /* Cuando en App Manual Paguen con Cheque */
    IF l-appsantander = FALSE AND l-TP = 61 THEN 
    DO:
        FIND FIRST ttPago WHERE ttPago.FechaCheque <> ? NO-LOCK NO-ERROR.
        IF AVAILABLE ttPago THEN  l-FecDep = ttPago.FechaCheque.    
    END.
    
    FIND Usuario WHERE Usuario.Id-User = l-UsuApl NO-LOCK NO-ERROR.
    CREATE Acuse.
    ASSIGN 
        Acuse.Id-Acuse      = l-FolAcuse
        Acuse.Id-Caja       = INTEGER(Usuario.Id-Caja)
        Acuse.Turno         = 1
        Acuse.Fecoper       = TODAY
        Acuse.FecReg        = TODAY
        Acuse.FecDep        = l-FecDep
        Acuse.UsuarioReg    = l-UsuApl  
        Acuse.Tipo          = IF l-appsantander = FALSE AND l-TP = 61 AND l-FecDep >= TODAY + 7 THEN "P" ELSE "N"   
        Acuse.Estatus       = IF l-appsantander = FALSE AND l-TP = 61 THEN 2 ELSE 4
        Acuse.Iniciales     = l-UsuApl
        Acuse.Id-Cajero     = Usuario.Id-Cajero
        Acuse.Id-Cliente    = IF AVAILABLE ttDocto THEN ttDocto.IdCliente ELSE 3
        Acuse.Id-Cobrador   = 25   // Numero Fijo (Validado en tabla Cobrador) 
        Acuse.AcuseCobrador = "0000000"
        Acuse.Comen[3]      = "".
    IF AVAILABLE DepBanco THEN 
    DO:
        ASSIGN 
            Acuse.Comen[1]  = "Acuse generado AUTOMATICAMENTE "
            Acuse.Comen[2]  = l-Comen2
            Acuse.Id-Origen = "ST".   // ST PARA SANTANDER
    END.
    ELSE 
    DO:
        ASSIGN 
            Acuse.Comen[1]  = "Acuse generado MANUALMENTE "
            Acuse.Comen[2]  = l-Comen2
            Acuse.Id-Origen = "SN".   
    END.          
    l-veces = 1.
    l-NPagos = 1.
    l-ImpPagado = 0.
    FOR EACH ttDocto WHERE ttDocto.IdCliente > 0 AND ttDocto.IdDoc > "" AND ttDocto.TDoc = "DEV" 
        NO-LOCK BY ttDocto.IdDoc:
        FIND Devolucion WHERE Devolucion.Id-Dev = INTEGER(ttDocto.IdDoc) 
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE Devolucion THEN NEXT.

        ASSIGN 
            Devolucion.Documento  = l-folacuse
            Devolucion.FecApl     = TODAY
            Devolucion.UsuarioApl = l-UsuApl.
        FIND Factura WHERE Factura.Id-Factura = Devolucion.Id-Factura NO-LOCK NO-ERROR.
        CREATE DocAcuse.
        ASSIGN
            DocAcuse.Id-Acuse   = l-folacuse
            DocAcuse.Documento  = Devolucion.Id-Factura  
            DocAcuse.FecDoc     = Devolucion.FecReg
            DocAcuse.Id-MC      = 65
            DocAcuse.Sec        = l-veces
            DocAcuse.Id-Moneda  = IF AVAILABLE Factura THEN Factura.Id-Moneda ELSE 1
            DocAcuse.TipoCambio = IF AVAILABLE Factura THEN Factura.TipoCambio ELSE 1
            DocAcuse.ImpDevol   = ttDocto.ImpPago
            DocAcuse.Id-Dev     = INTEGER(ttDocto.IdDoc)
            DocAcuse.ImpDescPP  = ttDocto.Desc9.
        l-ImpPagado         = l-ImpPagado - ttDocto.ImpPago.
        l-veces = l-veces + 1.
    
        CREATE PagoAcuse.
        ASSIGN
            PagoAcuse.Id-Acuse    = l-FolAcuse
            PagoAcuse.Sec         = l-NPagos 
            PagoAcuse.Id-Tp       = 65 
            PagoAcuse.ImpRecibido = DocAcuse.ImpDevol
            PagoAcuse.Importe     = DocAcuse.ImpDevol
            PagoAcuse.Id-Dev      = DocAcuse.Id-Dev
            PagoAcuse.Id-Moneda   = DocAcuse.Id-Moneda
            PagoAcuse.TC          = DocAcuse.TipoCambio     
            PagoAcuse.Id-Banco    = 0  
            PagoAcuse.Cuenta      = ""
            PagoAcuse.Cheque      = ""
            PagoAcuse.FecCheque   = ?    
            PagoAcuse.CPFormaPago = ""       // "01=Efectivo,02=Cheque,03=Transferencia"
            PagoAcuse.TipoCambio  = /*IF AVAILABLE TCReal THEN TCReal.Importe ELSE*/ 1.
        l-NPagos = l-NPagos + 1.
        RELEASE Devolucion.
    END.
    FOR EACH ttDocto WHERE ttDocto.IdCliente > 0 AND ttDocto.IdDoc > "" 
        AND  (ttDocto.TDoc = "FACT" OR ttDocto.TDoc = "CHEDEV")
        NO-LOCK BY ttDocto.IdDoc:
        
        IF ttDocto.TDoc = "FACT" THEN 
        DO:
            FIND Factura WHERE Factura.Id-Factura = ttDocto.IdDoc NO-LOCK NO-ERROR.
        END.
        ELSE 
        DO:
            IF ttDocto.TDoc = "CHEDEV" THEN
                FIND CheDev WHERE CheDev.Id-CheDev = ttDocto.IdDoc NO-LOCK NO-ERROR.
        END. 
   
        l-AntApl = 0.
        FOR EACH ttAnticipos WHERE ttAnticipos.IdDoc = ttDocto.IdDoc NO-LOCK:
            l-AntApl = ttAnticipos.ImpAplicado.
            FIND Anticipo WHERE Anticipo.Id-Anticipo = ttAnticipos.IdAnticipo   
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE Anticipo THEN 
            DO:
                l-Nda = 0.
                FIND LAST DetAnticipo WHERE DetAnticipo.Id-Anticipo = Anticipo.Id-Anticipo
                    NO-LOCK NO-ERROR.
                IF AVAILABLE DetAnticipo THEN
                    ASSIGN l-Nda = DetAnticipo.Sec.
                CREATE DetAnticipo.
                ASSIGN 
                    DetAnticipo.Id-Anticipo = Anticipo.Id-Anticipo
                    DetAnticipo.FecReg      = TODAY
                    DetAnticipo.Documento   = ttDocto.IdDoc
                    DetAnticipo.Id-MC       = 1
                    DetAnticipo.Importe     = ttAnticipos.ImpAplicado
                    DetAnticipo.Sec         = l-Nda + 1
                    DetAnticipo.UsuarioApl  = l-UsuApl.
                ASSIGN 
                    Anticipo.ImpAplicado = Anticipo.ImpAplicado + DetAnticipo.Importe.
            END.
            RELEASE Anticipo.        
        END.
        CREATE DocAcuse.
        ASSIGN
            DocAcuse.Id-Acuse   = l-folacuse
            DocAcuse.Documento  = ttDocto.IdDoc  
            DocAcuse.FecDoc     = IF ttDocto.TDoc = "FACT" THEN Factura.FecReg   ELSE CheDev.FecCargo
            DocAcuse.Id-MC      = IF ttDocto.TDoc = "FACT" THEN 1  ELSE 3   
            DocAcuse.Sec        = l-veces
            DocAcuse.Id-Moneda  = IF ttDocto.TDoc = "FACT" THEN Factura.Id-Moneda  ELSE 1
            DocAcuse.TipoCambio = IF ttDocto.TDoc = "FACT" THEN Factura.TipoCambio ELSE 1
            DocAcuse.ImpPago    = ttDocto.ImpPago
            DocAcuse.ImpDescPP  = ttDocto.Desc9    // Descuento Pronto Pago  
            DocAcuse.ImpDescEsp = ttDocto.Desc10
            DocAcuse.MDDesc     = ttDocto.Desc1
            DocAcuse.FMDesc     = ttDocto.Desc2
            DocAcuse.MRDesc     = ttDocto.Desc3
            DocAcuse.UPDesc     = ttDocto.Desc4
            DocAcuse.ManDesc    = ttDocto.Desc5  
            DocAcuse.DPDesc     = ttDocto.Desc6
            DocAcuse.MTDesc     = ttDocto.Desc7
            DocAcuse.RebDesc    = ttDocto.Desc8
            DocAcuse.MenorDesc  = ttDocto.Desc11
            DocAcuse.CBDesc     = ttDocto.Desc12
            DocAcuse.AntApl     = l-AntApl
            l-ImpPagado         = l-ImpPagado + ttDocto.ImpPago.

        l-veces = l-veces + 1. 
    
 
    END.   
    
    /* Cambiar Calidad cuando es un documento Chedev */ 
    FIND FIRST ttDocto WHERE ttDocto.TDoc = "CHEDEV" NO-LOCK NO-ERROR.
    IF AVAILABLE ttDocto THEN 
    DO :
        FIND FIRST Cliente WHERE Cliente.Id-Cliente = ttDocto.IdCliente
            AND  Cliente.Id-Calidad = 35  NO-ERROR.
        IF AVAILABLE Cliente THEN 
        DO:
            FIND FIRST CambioCte NO-LOCK WHERE CambioCte.Id-Cliente = ttDocto.IdCliente
                AND CambioCte.Campo = 505
                AND CambioCte.ValorOld <> "35".
            IF AVAILABLE CambioCte THEN 
            DO:
                ASSIGN 
                    Cliente.Id-Calidad = INT(CambioCte.ValorOld)
                    l-ValorOld         = CambioCte.ValorOld
                    l-ValorNuevo       = CambioCte.ValorNuevo.
                
                CREATE CambioCte.
                ASSIGN
                    CambioCte.Id-Cliente = ttDocto.IdCliente
                    CambioCte.Id-User    = ttDocto.IdUser 
                    CambioCte.Descr      = "Id-Calidad"
                    CambioCte.ValorNuevo = l-ValorOld
                    CambioCte.ValorOld   = l-ValorNuevo
                    CambioCte.FecReg     = TODAY
                    CambioCte.Hora       = TIME
                    CambioCte.Campo      = 505.
            END.  
        END.    
    END.             

    l-TP = 0.  
    l-ImpDeposito = 0.   
    FIND LAST TCReal WHERE TCReal.Id-Moneda = 3 NO-LOCK NO-ERROR.
    FOR EACH ttPago WHERE ttPago.Importe > 0 BY ttPago.Importe DESCENDING:
        IF l-TP = 0 THEN
            l-TP = ttPago.FormaPago.
        IF l-descsantander = "DEP S B COBRO" THEN l-TP = 61. 
        IF l-descsantander = "DEP S B COBR" THEN l-TP = 61.
        ASSIGN 
            ip-formapago = "03".
        IF l-TP = 52 THEN
            ASSIGN ip-formapago = "28".
        ELSE IF l-TP = 57 OR l-TP = 58 THEN
                ASSIGN ip-formapago = "03".
            ELSE IF l-TP = 60 THEN
                    ASSIGN ip-formapago = "01".
                ELSE IF l-TP = 61 THEN
                        ASSIGN ip-formapago = "02".
                    ELSE IF l-TP = 62 THEN
                            ASSIGN ip-formapago = "04".
       
        CREATE PagoAcuse.
        ASSIGN
            PagoAcuse.Id-Acuse    = l-FolAcuse
            PagoAcuse.Sec         = l-NPagos 
            PagoAcuse.Id-Tp       = l-TP   // ttPago.FormaPago    
            PagoAcuse.ImpRecibido = ttPago.Importe
            PagoAcuse.Importe     = ttPago.Importe
           // PagoAcuse.Id-Banco    = IF ttPago.IdBanco <> 0 THEN ttPago.IdBanco ELSE 25 
            PagoAcuse.Id-Banco    = IF ttPago.IdBanco <> 0 THEN ttPago.IdBanco 
                                    ELSE IF l-TP = 57 THEN 25
                                    ELSE IF l-TP = 58 THEN 1
                                    ELSE 0  
            PagoAcuse.Cuenta      = IF ttPago.Cuenta <> "0" THEN ttPago.Cuenta  ELSE ""  
            PagoAcuse.Cheque      = IF ttPago.FolioCheque <> "0" THEN ttPago.FolioCheque ELSE ""
            PagoAcuse.FecCheque   = IF ttPago.FechaCheque <> ? THEN ttPago.FechaCheque ELSE ?      
            PagoAcuse.CPFormaPago = ip-formapago       // "01=Efectivo,02=Cheque,03=Transferencia"
            PagoAcuse.Id-Moneda   = 1
            PagoAcuse.TC          = 1     
            PagoAcuse.TipoCambio  = /*IF AVAILABLE TCReal THEN TCReal.Importe ELSE*/ 1.
  //  l-ImpDeposito = l-ImpDeposito + PagoAcuse.Importe.
        l-ImpDeposito = l-ImpDeposito + ttPago.Importe.       
        l-NPagos = l-NPagos + 1.
        IF ttPago.Rec <> 0 AND AVAILABLE DepBanco THEN 
        DO: // le puse diferente a 0 para la aplicacion de pago manual
            ASSIGN 
                DepBanco.Id-Acuse   = Acuse.Id-Acuse
                DepBanco.Conciliado = TRUE
                DepBanco.FecAplica  = TODAY
                DepBanco.Id-User    = l-UsuApl.  
        END.    
    END.   
    ASSIGN 
        p-Acuse = l-FolAcuse.   
        
    /* Primero validamos la condición que debe hacer que NO se ejecute el proceso */
    IF NOT (l-appsantander = FALSE AND l-TP = 61) THEN 
    DO:
        /* Solo si NO se cumple la condición, ejecutamos el proceso completo */
        FOR EACH DocAcuse OF Acuse NO-LOCK:
 
            IF DocAcuse.impPago > 0 THEN 
            DO: 
            {programas/concacuse.i      
            &TipoMov      = l-TP 
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = TRUE
            &Importe      = " ( DocAcuse.ImpPago * -1 ) " }
            END.
            IF DocAcuse.impdescPP > 0 THEN 
            DO:
            {programas/concacuse.i 
            &TipoMov      = 63 
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = TRUE
            &Importe      = " ( DocAcuse.impdescPP * -1 ) " }
            END.
            IF DocAcuse.impdescesp > 0 THEN 
            DO:
            {programas/concacuse.i
            &TipoMov      = 68
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = TRUE
            &Importe      = " ( DocAcuse.impdescEsp * -1 ) " }
            END.
            IF DocAcuse.MDDesc > 0 THEN 
            DO:
            {programas/concacuse.i
            &TipoMov      = 70
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = TRUE
            &Importe      = " ( DocAcuse.MDDesc * -1 ) " }
            END.
            IF DocAcuse.FMDesc > 0 THEN 
            DO:
            {programas/concacuse.i
            &TipoMov      = 69
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = TRUE
            &Importe      = " ( DocAcuse.FMDesc * -1 ) " }
            END.
            IF DocAcuse.MRDesc > 0 THEN 
            DO:
            {programas/concacuse.i
            &TipoMov      = 71
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = TRUE
            &Importe      = " ( DocAcuse.MRDesc * -1 ) " }
            END.
            IF DocAcuse.UPDesc > 0 THEN 
            DO:
            {programas/concacuse.i
            &TipoMov      = 72
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = TRUE
            &Importe      = " ( DocAcuse.UPDesc * -1 ) " }
            END.
            IF DocAcuse.ManDesc > 0 THEN 
            DO:
            {programas/concacuse.i
            &TipoMov      = 74
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = TRUE
            &Importe      = " ( DocAcuse.ManDesc * -1 ) " }
            END.
            IF DocAcuse.DPDesc > 0 THEN 
            DO:
            {programas/concacuse.i
            &TipoMov      = 66
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = TRUE
            &Importe      = " ( DocAcuse.DPDesc * -1 ) " }
            END.
            IF DocAcuse.MTDesc > 0 THEN 
            DO:
            {programas/concacuse.i
            &TipoMov      = 75
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = TRUE
            &Importe      = " ( DocAcuse.MTDesc * -1 ) " }
            END.
            IF DocAcuse.RebDesc > 0 THEN 
            DO:
            {programas/concacuse.i
            &TipoMov      = 76
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = TRUE
            &Importe      = " ( DocAcuse.RebDesc * -1 ) " }
            END.
            IF DocAcuse.MenorDesc > 0 THEN 
            DO:
            {programas/concacuse.i 
            &TipoMov      = 99
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = TRUE
            &Importe      = " ( DocAcuse.MenorDesc * -1 ) " }
            END.
            IF DocAcuse.CBDesc > 0 THEN 
            DO:
            {programas/concacuse.i
            &TipoMov      = 79
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = TRUE
            &Importe      = " ( DocAcuse.CBDesc * -1 ) " }
            END.
            IF DocAcuse.AntApl > 0 THEN 
            DO:
            {programas/concacuse.i
            &TipoMov      = 90
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = TRUE
            &Importe      = " ( DocAcuse.AntApl * -1 ) " }   
            END.      
        END.   
    END. /* FIN DEL IF NOT (l-appsantander = FALSE AND l-TP = 61) */
    IF l-appsantander = FALSE AND l-TP = 61 THEN DO:      
        FOR EACH DocAcuse OF Acuse NO-LOCK:
 
            IF DocAcuse.impPago > 0 THEN 
            DO: 
            {programas/concacuse.i      
            &TipoMov      = l-TP 
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = FALSE
            &Importe      = " ( DocAcuse.ImpPago * -1 ) " }
            END.
            IF DocAcuse.impdescPP > 0 THEN 
            DO:
            {programas/concacuse.i 
            &TipoMov      = 63 
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = FALSE
            &Importe      = " ( DocAcuse.impdescPP * -1 ) " }
            END.
            IF DocAcuse.impdescesp > 0 THEN 
            DO:
            {programas/concacuse.i
            &TipoMov      = 68
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = FALSE
            &Importe      = " ( DocAcuse.impdescEsp * -1 ) " }
            END.
            IF DocAcuse.MDDesc > 0 THEN 
            DO:
            {programas/concacuse.i
            &TipoMov      = 70
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = FALSE
            &Importe      = " ( DocAcuse.MDDesc * -1 ) " }
            END.
            IF DocAcuse.FMDesc > 0 THEN 
            DO:
            {programas/concacuse.i
            &TipoMov      = 69
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = FALSE
            &Importe      = " ( DocAcuse.FMDesc * -1 ) " }
            END.
            IF DocAcuse.MRDesc > 0 THEN 
            DO:
            {programas/concacuse.i
            &TipoMov      = 71
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = FALSE
            &Importe      = " ( DocAcuse.MRDesc * -1 ) " }
            END.
            IF DocAcuse.UPDesc > 0 THEN 
            DO:
            {programas/concacuse.i
            &TipoMov      = 72
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = FALSE
            &Importe      = " ( DocAcuse.UPDesc * -1 ) " }
            END.
            IF DocAcuse.ManDesc > 0 THEN 
            DO:
            {programas/concacuse.i
            &TipoMov      = 74
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = FALSE
            &Importe      = " ( DocAcuse.ManDesc * -1 ) " }
            END.
            IF DocAcuse.DPDesc > 0 THEN 
            DO:
            {programas/concacuse.i
            &TipoMov      = 66
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = FALSE
            &Importe      = " ( DocAcuse.DPDesc * -1 ) " }
            END.
            IF DocAcuse.MTDesc > 0 THEN 
            DO:
            {programas/concacuse.i
            &TipoMov      = 75
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = FALSE
            &Importe      = " ( DocAcuse.MTDesc * -1 ) " }
            END.
            IF DocAcuse.RebDesc > 0 THEN 
            DO:
            {programas/concacuse.i
            &TipoMov      = 76
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = FALSE
            &Importe      = " ( DocAcuse.RebDesc * -1 ) " }
            END.
            IF DocAcuse.MenorDesc > 0 THEN 
            DO:
            {programas/concacuse.i 
            &TipoMov      = 99
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = FALSE
            &Importe      = " ( DocAcuse.MenorDesc * -1 ) " }
            END.
            IF DocAcuse.CBDesc > 0 THEN 
            DO:
            {programas/concacuse.i
            &TipoMov      = 79
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = FALSE
            &Importe      = " ( DocAcuse.CBDesc * -1 ) " }
            END.
            IF DocAcuse.AntApl > 0 THEN 
            DO:
            {programas/concacuse.i
            &TipoMov      = 90
            &TipoPadre    = DocAcuse.id-MC
            &FecReg       = Acuse.FecDep
            &Documento    = DocAcuse.Id-Acuse
            &RefSaldo     = DocAcuse.documento
            &Cliente      = Acuse.Id-Cliente
            &Afectar      = FALSE
            &Importe      = " ( DocAcuse.AntApl * -1 ) " }   
            END.      
        END.      
        
    END.           
   
    l-RestoAnt = l-depsantander - l-ImpPagado.   
// l-RestoAnt = l-ImpDeposito - l-ImpPagado.
    IF l-RestoAnt > 10 THEN 
    DO:
        FIND Folio WHERE Folio.Id-Doc = "ACUSE" AND Folio.Id-Alm = "NA" 
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE Folio THEN 
        DO:
            CREATE Folio.
            ASSIGN 
                Folio.Id-Doc  = "ACUSE"
                Folio.Id-Alm  = "NA"
                Folio.Prefijo = "NA"
                Folio.Folio   = 1.
        END.
    
        ASSIGN 
            l-folAntacuse = STRING(Folio.Folio,"9999999") + TRIM(Folio.PreFijo)
            l-folAntacuse = SUBSTRING(l-folAntacuse,LENGTH(l-folAntacuse) - 6,7)
            Folio.Folio   = Folio.Folio + 1.
        RELEASE Folio.  
    
        FIND FIRST ttDocto WHERE ttDocto.IdCliente > 0 NO-LOCK NO-ERROR.
        CREATE Acuse.
        ASSIGN 
            Acuse.Id-Acuse      = l-FolAntAcuse
            Acuse.Id-Caja       = INTEGER(Usuario.Id-Caja)
            Acuse.Turno         = 1
            Acuse.Fecoper       = TODAY
            Acuse.FecReg        = TODAY
            Acuse.FecDep        = l-FecDep
            Acuse.UsuarioReg    = l-UsuApl
            Acuse.Tipo          = "A"
            Acuse.Estatus       = 4
            Acuse.Iniciales     = l-UsuApl
            Acuse.Id-Cajero     = Usuario.Id-Cajero
            Acuse.Id-Cliente    = IF AVAILABLE ttDocto THEN ttDocto.IdCliente ELSE 3
            Acuse.Id-Cobrador   = 25   // Numero Fijo (Validado en tabla Cobrador) 
            Acuse.AcuseCobrador = "0000000"
            Acuse.Comen[3]      = "Sobrante de pago con Acuse " + l-folAcuse.
        IF AVAILABLE DepBanco THEN 
        DO:
            ASSIGN 
                Acuse.Comen[1]  = "Acuse generado AUTOMATICAMENTE "
                Acuse.Comen[2]  = l-Comen2
                Acuse.Id-Origen = 'ST'.
        END.
        ELSE 
        DO:
            ASSIGN 
                Acuse.Comen[1]  = "Acuse generado MANUALMENTE "
                Acuse.Comen[2]  = ""
                Acuse.Id-Origen = 'SN' .  
        END.       
        CREATE PagoAcuse.
        ASSIGN
            PagoAcuse.Id-Acuse    = l-FolAntAcuse
            PagoAcuse.Sec         = 1 
            PagoAcuse.Id-Tp       = l-TP 
            PagoAcuse.ImpRecibido = l-RestoAnt
            PagoAcuse.Importe     = l-RestoAnt
         // PagoAcuse.Id-Banco    = 25  
            PagoAcuse.Id-Banco    = IF l-TP = 57 THEN 25
                                    ELSE IF l-TP = 58 THEN 1
                                    ELSE 25   
            PagoAcuse.Cuenta      = ""
            PagoAcuse.Cheque      = ""
            PagoAcuse.FecCheque   = ?    
            PagoAcuse.CPFormaPago = ip-formapago       // "01=Efectivo,02=Cheque,03=Transferencia"
            PagoAcuse.Id-Moneda   = 1
            PagoAcuse.TC          = 1     
            PagoAcuse.TipoCambio  = /*IF AVAILABLE TCReal THEN TCReal.Importe ELSE*/ 1.

        FIND FIRST ttPago WHERE ttPago.Rec <> 0 NO-LOCK NO-ERROR.
        IF AVAILABLE ttPago AND AVAILABLE DepBanco THEN 
        DO:
            FIND CURRENT DepBanco EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN 
                DepBanco.Id-AcuseAnt = l-folAntAcuse.
            FIND CURRENT DepBanco NO-LOCK NO-ERROR.
        END.    
        
        FIND Folio WHERE Folio.Id-Doc = "ANT" AND Folio.Id-Alm = ""
            EXCLUSIVE-LOCK NO-ERROR.
        CREATE Anticipo.
        ASSIGN
            Anticipo.Id-Anticipo = STRING(Folio.Folio,'9999999')
            Anticipo.Id-Cliente  = Acuse.Id-Cliente
            Anticipo.Id-Acuse    = Acuse.Id-Acuse
            Anticipo.FecReg      = TODAY
            Anticipo.ImpAnticipo = l-RestoAnt
            Anticipo.ImpAplicado = 0
            Anticipo.ImpDevuelto = 0
            Anticipo.ImpContado  = 0
            Anticipo.Canc        = FALSE   
            Anticipo.Concepto    = Acuse.Comen
            Folio.Folio          = Folio.Folio + 1.
        RELEASE Folio.      
    END.
    


/* Llamar al procedimiento antes de retornar */
FIND CURRENT Folio EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE Folio THEN RELEASE Folio.
    
    /* Tablas específicas de procesos */
    
    IF AVAILABLE Devolucion THEN RELEASE Devolucion.   
    IF AVAILABLE Anticipo   THEN RELEASE Anticipo.
    IF AVAILABLE MovCliente THEN RELEASE MovCliente.
    IF AVAILABLE Acuse      THEN RELEASE Acuse.
    IF AVAILABLE DepBanco   THEN RELEASE DepBanco.  
    IF AVAILABLE DocAcuse   THEN RELEASE DocAcuse.
    IF AVAILABLE PagoAcuse  THEN RELEASE PagoAcuse. 
    
    /* Mensaje de log para depuración */
    MESSAGE "Tablas liberadas para acuse:" l-FolAcuse VIEW-AS ALERT-BOX.
       
    RETURN.    

END PROCEDURE. 
     