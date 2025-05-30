@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : carteractevisor.p
    Purpose     : /MovimientoCliente

    Syntax      :

    Description : 

    Author(s)   : sis10
    Created     : Tue Oct 22 14:47:26 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE l-num          AS INT.  
DEFINE VARIABLE l-tot-total    AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-vig      AS DECIMAL   FORMAT ">>>,>>>,>>9.99". 
DEFINE VARIABLE l-tot-ven      AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-porv     AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-clase        AS CHARACTER FORMAT "X(20)". 
DEFINE VARIABLE l-segmento     AS CHARACTER FORMAT "X(20)".
DEFINE VARIABLE l-estatus      AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE l-moneda       AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE l-tipo-moneda  AS INT.
DEFINE VARIABLE l-tot-30       AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-31       AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-61       AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-tot-91       AS DECIMAL   FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-dia          AS INT.
DEFINE VARIABLE l-dia-max      AS INT       FORMAT "zz9" NO-UNDO.


DEF    VAR      l-cargo        AS DECI      FORMAT ">,>>>,>>9.99" LABEL "Cargo" NO-UNDO.
DEF    VAR      l-credito      AS DECI      FORMAT ">,>>>,>>9.99" LABEL "Credito" NO-UNDO.
DEF    VAR      l-totcargo     AS DECI      NO-UNDO.
DEF    VAR      l-totcredito   AS DECI      NO-UNDO.
DEF    VAR      l-cargoME      AS DECI      FORMAT ">,>>>,>>9.99" NO-UNDO.
DEF    VAR      l-creditoME    AS DECI      FORMAT ">,>>>,>>9.99" NO-UNDO.
DEF    VAR      l-vencidoME    AS DECI      FORMAT "-zzzzz9.99" NO-UNDO.   // RNPC - 2019-07-08 
DEF    VAR      l-pvencerME    AS DECI      FORMAT "-zzzzz9.99" NO-UNDO.   // RNPC - 2019-07-08
DEF    VAR      l-saldoME      AS DECI      FORMAT "-zzzzz9.99" NO-UNDO.   // RNPC - 2019-07-08
DEF    VAR      l-tot30ME      AS DECI      FORMAT "-zzzzz9.99" NO-UNDO.   // RNPC - 2019-07-08
DEF    VAR      l-tot90ME      AS DECI      FORMAT "-zzzzz9.99" NO-UNDO.   // RNPC - 2019-07-08
DEF    VAR      l-tot91ME      AS DECI      FORMAT "-zzzzz9.99" NO-UNDO.   // RNPC - 2019-07-08
DEF    VAR      l-totcargoME   AS DECI      NO-UNDO.
DEF    VAR      l-totcreditoME AS DECI      NO-UNDO.
DEF    VAR      l-RefValor     AS CHARACTER NO-UNDO.
DEF    VAR      l-vencido      AS DECI      FORMAT "-zzzzz9.99" LABEL "Vencido" NO-UNDO.
DEF    VAR      l-pvencer      AS DECI      FORMAT "-zzzzz9.99" LABEL "P/Vencer" NO-UNDO.
DEF    VAR      l-saldo        AS DECI      FORMAT "-zzzzz9.99" LABEL "Saldo" NO-UNDO.
DEF    VAR      l-tot30        AS DECI      FORMAT "-zzzzz9.99" LABEL " 1-30" NO-UNDO.
DEF    VAR      l-tot60        AS DECI      FORMAT "-zzzzz9.99" LABEL "31-60" NO-UNDO.
DEF    VAR      l-tot90        AS DECI      FORMAT "-zzzzz9.99" LABEL "61+" NO-UNDO.
DEF    VAR      l-totMas90     AS DECI      FORMAT "-zzzzz9.99" LABEL "90+" NO-UNDO.
DEF    VAR      l-chequepag    AS INTE      FORMAT ">9" NO-UNDO.
DEFINE VARIABLE l-prompago     AS DECIMAL.
DEF    VAR      l-pagina       AS INTE      FORMAT "zz9" NO-UNDO.
DEF    VAR      l-numcheque    AS INTE      FORMAT "z9" NO-UNDO.
DEF    VAR      l-consecutivo  AS INTE      NO-UNDO.
DEF    VAR      l-HayMonedaEX  AS LOGICAL   INITIAL FALSE NO-UNDO.   // RNPC - 2019-07-08
DEF    VAR      l-largo        AS INTE      INITIAL 20 NO-UNDO.
DEF    VAR      l-tam          AS INTE      NO-UNDO.
DEF    VAR      l-ctrl         AS INTE      NO-UNDO.
DEF    VAR      l-cont         AS INTE      NO-UNDO.
DEF    VAR      l-dias         AS INTE      FORMAT "zz9" NO-UNDO.
DEF    VAR      l-linea2       AS INTE      INITIAL 0 NO-UNDO.
DEF    VAR      l-ant          AS INTE      FORMAT "zz9" LABEL "Ant" NO-UNDO.
DEF    VAR      l-resp         AS LOGI      FORMAT "Si/No" NO-UNDO.
DEF    VAR      l-hubo         AS LOGI      NO-UNDO.
DEF    VAR      l-acomodo      AS INTE      NO-UNDO.
DEFINE TEMP-TABLE ttCliente NO-UNDO    
    FIELD IdCliente    LIKE Cliente.Id-Cliente
    FIELD RazonSocial  LIKE Cliente.RazonSocial
    FIELD Telefono     LIKE Cliente.Tel1
    FIELD CalleNo      LIKE Cliente.CalleNo
    FIELD NumExterior  LIKE Cliente.NumExt
    FIELD Colonia      LIKE Cliente.Colonia
    FIELD Estatus      LIKE Cliente.Activo
    FIELD LineaCredito LIKE Cliente.Limite
    FIELD Plazo        LIKE Cliente.Plazo
    FIELD IdVendedor   LIKE Cliente.Id-Vendedor
    FIELD Vendedor     AS CHARACTER
    FIELD IdResp       LIKE Cliente.Id-Resp
    FIELD Resp         LIKE Resp.Nombre
    FIELD IdCob        LIKE Cliente.Id-Cobrador           
    FIELD Cobrador     AS CHARACTER 
    FIELD CteDig       AS CHARACTER
    INDEX idx-clase IdCliente ASCENDING.

DEFINE TEMP-TABLE ttDetalle NO-UNDO 
    FIELD IdCliente    LIKE MovCliente.Id-Cliente
    FIELD Documento    LIKE MovCliente.RefSaldo
    FIELD Fecha        LIKE MovCliente.FecReg
    FIELD PlazoFactura AS INT 
    FIELD Descripcion  AS CHAR
    FIELD Cargo        AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD Credito      AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD Saldo        LIKE MovCliente.Saldo
    FIELD Antiguedad   AS INT
    FIELD Referencia   AS CHAR
    FIELD TipoAcuse    LIKE Acuse.Tipo   
    FIELD FolioEstatus LIKE Factura.CteEstatus
    FIELD Acuse        LIKE Acuse.Id-Acuse
    FIELD Registro     LIKE MovCliente.FecReg
    FIELD Margen       AS DECIMAL FORMAT "-zzz9.99%"      
    FIELD Acomodo      AS INTE 
    FIELD Rec          AS RECID    
    FIELD Id-MC        LIKE MovCliente.Id-MC
    INDEX Idx-Acomodo Acomodo .
    
DEFINE TEMP-TABLE ttCartera NO-UNDO
    FIELD id             AS INTEGER
    FIELD IdCliente      AS INTEGER
    FIELD saldo          AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD porvencer      AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD montovencido   AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD treinta        AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD sesenta        AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD noventa        AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD noventamas     AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD diacartera     AS INTEGER
    FIELD promedio       AS DECIMAL  
    FIELD margenpromedio AS DECIMAL
    INDEX idx-clase IdCliente ASCENDING.
/* Definir el DATASET con relaciones */ 
DEFINE DATASET dsMov FOR 
    ttCliente, /* Tabla principal */
    ttDetalle, /* Relación con Cliente */
    ttCartera  /* Relación con Cliente */
    DATA-RELATION ClienteDetalle FOR ttCliente, ttDetalle
    RELATION-FIELDS (IdCliente, IdCliente) /* Relación por IdCliente */
    DATA-RELATION ClienteCartera FOR ttCliente, ttCartera
    RELATION-FIELDS (IdCliente, IdCliente). /* Relación por IdCliente */


DEF BUFFER b-mov     FOR MovCliente.

DEF BUFFER bk-saldo  FOR ttDetalle.
DEF BUFFER bbk-saldo FOR ttDetalle.
DEF    VAR      l-fecha   AS DATE    NO-UNDO.
DEFINE VARIABLE v-marneto AS DECIMAL NO-UNDO FORMAT "-zzz9.99%".
DEFINE VARIABLE v-margen  AS DECIMAL NO-UNDO FORMAT "-zzz9.99%".

DEF VAR l-digver    AS CHAR .
DEF VAR l-CteDig    AS INTEGER NO-UNDO.
/* **********************  Internal Procedures  *********************** */


@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetCartera:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    DEFINE INPUT PARAMETER  lCliente  AS INT.
    DEFINE OUTPUT PARAMETER Respuesta AS CHAR. 
    DEFINE OUTPUT PARAMETER DATASET FOR dsMov.

    ASSIGN 
        l-num    = 0
        l-moneda = "".   

    EMPTY TEMP-TABLE ttCartera.
    
    FIND FIRST Cliente WHERE Cliente.Id-Cliente = lCliente NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Cliente OR lCliente = 0 THEN
    DO:
        ASSIGN 
            Respuesta = "El cliente NO Existe".
        RETURN.
    END. 
    IF Cliente.Activo = FALSE THEN 
    DO:
        FIND FIRST Usuario WHERE Usuario.Id-User = Cliente.Id-User NO-LOCK NO-ERROR.
        /* Construir la cadena */
        ASSIGN 
            Respuesta = "Cuenta inactivada por: " + (IF AVAILABLE Usuario THEN CAPS(Usuario.Nom-usuario) ELSE "") + " " 
                          + "Fecha: " + (IF Cliente.FecBaja <> ? 
                                        THEN STRING(Cliente.FecBaja, "99/99/9999") 
                                        ELSE "Sin fecha") + "          ".
        RETURN.
    END.   
    IF AVAILABLE Cliente THEN 
    DO :
        RUN programas/vtad1000.p(INPUT Cliente.Id-Cliente, OUTPUT l-CteDig).
        ASSIGN l-digver = STRING(Cliente.Id-Cliente) + "-" + STRING(l-CteDig,"99").  
        CREATE ttCliente.
        ASSIGN   
            ttCliente.IdCliente    = Cliente.Id-Cliente  
            ttCliente.RazonSocial  = Cliente.RazonSocial
            ttCliente.Telefono     = Cliente.Tel1 + " " + Cliente.Tel2 + " " + Cliente.Tel3
            ttCliente.CalleNo      = Cliente.CalleNo
            ttCliente.NumExterior  = Cliente.NumExt
            ttCliente.Colonia      = Cliente.Colonia
            ttCliente.Estatus      = Cliente.Activo
            ttCliente.LineaCredito = Cliente.Limite
            ttCliente.Plazo        = Cliente.Plazo
            ttCliente.IdVendedor   = Cliente.Id-Vendedor
            ttCliente.IdResp       = Cliente.Id-Resp
            ttCliente.CteDig       =  l-digver .
   
        FIND FIRST Resp WHERE Resp.Id-Resp = Cliente.Id-Resp NO-LOCK NO-ERROR.
        FIND FIRST Cobrador WHERE Cobrador.Id-Cobrador = Cliente.Id-Cobrador NO-LOCK NO-ERROR.
        FIND FIRST Vendedor WHERE Vendedor.Id-Vendedor = Cliente.Id-Vendedor NO-LOCK NO-ERROR.
        IF AVAILABLE Vendedor THEN 
        DO:
            FIND FIRST empleado WHERE empleado.Iniciales = Vendedor.Iniciales NO-LOCK NO-ERROR.
        END.
        ASSIGN
            ttCliente.Vendedor = empleado.Nombre 
            WHEN AVAILABLE empleado
            ttCliente.Resp     = Resp.Nombre 
            WHEN AVAILABLE Resp
            ttCliente.IdCob    = Cliente.Id-Cobrador
            ttCliente.Cobrador = Cobrador.Nombre 
            WHEN AVAILABLE Cobrador. . 
    END.  
    
    /**************************************************************************************************** */
     
    FOR EACH b-mov WHERE b-mov.Id-Cliente = lCliente  AND
        b-mov.FecReg    <= TODAY AND
        b-mov.Id-MC     <= 3 AND
        b-mov.Afectado  NO-LOCK
        BREAK BY b-mov.FecReg
        BY b-mov.RefSaldo
        BY b-mov.Id-MC:
        IF b-mov.Id-MC <= 3 THEN 
        DO:
            FOR EACH MovCliente WHERE MovCliente.RefSaldo = b-mov.RefSaldo  AND
                MovCliente.FecReg  <= TODAY      AND
                MovCliente.Id-MC    > 3               AND
                MovCliente.Afectado NO-LOCK:
                ACCUMULATE MovCliente.Importe (TOTAL).
            END.
            ASSIGN 
                l-saldo = b-mov.Importe + ACCUM TOTAL MovCliente.Importe.
        
        END. /* del MovCliente.Id-MC <= 3 */
        FOR EACH MovCliente WHERE MovCliente.RefSaldo = b-Mov.refsaldo AND
            MovCliente.FecReg  <= TODAY     AND
            MovCliente.Afectado NO-LOCK
            BREAK BY MovCliente.REfSaldo
            BY MovCliente.Id-MC:
            FIND TabMC OF MovCliente NO-LOCK NO-ERROR.
            FIND Cliente WHERE Cliente.Id-Cliente = MovCliente.Id-Cliente
                NO-LOCK NO-ERROR.
            IF MovCliente.Id-Mc <> 65 THEN
                FIND Acuse WHERE Acuse.Id-Acuse = MovCliente.Documento NO-LOCK NO-ERROR.
            ELSE RELEASE Acuse.

        
            IF FIRST-OF(MovCliente.RefSaldo) THEN 
            DO:
                ASSIGN 
                    l-fecha = MovCliente.FecReg.
            END.
        
            ASSIGN 
                l-cargo   = (IF MovCliente.Importe > 0 THEN MovCliente.Importe ELSE 0)
                l-credito = (IF MovCliente.Importe < 0 THEN MovCliente.Importe ELSE 0).
                   
            ACCUMULATE l-cargo (TOTAL BY MovCliente.RefSaldo).
            ACCUMULATE l-credito (TOTAL BY MovCliente.RefSaldo).
        
            /*_ RNPC - Sumatoria de totales seg�n la moneda _*/
            IF MovCliente.Id-Moneda > 1 THEN 
            DO:
                ASSIGN 
                    l-cargoME      = (IF MovCliente.Importe > 0 THEN MovCliente.Importe ELSE 0)
                    l-creditoME    = (IF MovCliente.Importe < 0 THEN MovCliente.Importe ELSE 0)
                    l-totcargoME   = l-totcargoME + (l-cargo)
                    l-totcreditoME = l-totcreditoME + (l-credito).
            
                IF MovCliente.FecVenc >= TODAY AND MovCliente.Id-MC <= 3 THEN
                    ASSIGN l-pvencerME = l-pvencerME + l-saldo.
                IF MovCliente.Id-MC <= 3 AND MovCliente.FecVenc <= TODAY THEN 
                DO:
                    IF (TODAY - MovCliente.FecVenc) <= 30 AND
                        (TODAY - MovCliente.FecVenc) >= 1 THEN
                        ASSIGN l-tot30ME = l-tot30ME + l-saldo.
                    IF (TODAY - MovCliente.FecVenc) <= 60 AND
                        (TODAY - MovCliente.FecVenc) >= 31 THEN
                        ASSIGN l-tot90ME = l-tot90ME + l-saldo.
                    IF (TODAY - MovCliente.FecVenc) >= 61 THEN
                        ASSIGN l-tot91ME = l-tot91ME + l-saldo.
                END.
            END.
            ELSE 
            DO:
                ASSIGN 
                    l-totcargo   = l-totcargo + l-cargo
                    l-totcredito = l-totcredito + l-credito.
            
                IF MovCliente.FecVenc >= TODAY AND MovCliente.Id-MC <= 3 THEN
                    ASSIGN l-pvencer = l-pvencer + l-saldo.
                IF MovCliente.Id-MC <= 3 AND MovCliente.FecVenc <= TODAY THEN 
                DO:
                    IF (TODAY - MovCliente.FecVenc) >= 1 AND (TODAY - MovCliente.FecVenc) <= 30 THEN 
                    DO:
                        ASSIGN 
                            l-tot30 = l-tot30 + l-saldo.      /* 1-30 días */
                    END.
                    ELSE IF (TODAY - MovCliente.FecVenc) >= 31 AND (TODAY - MovCliente.FecVenc) <= 60 THEN 
                        DO:
                            ASSIGN 
                                l-tot60 = l-tot60 + l-saldo.      /* 31-60 días */
                        END.
                        ELSE IF (TODAY - MovCliente.FecVenc) >= 61 AND (TODAY - MovCliente.FecVenc) <= 90 THEN 
                            DO:
                                ASSIGN 
                                    l-tot90 = l-tot90 + l-saldo.      /* 61-90 días */
                            END.
                            ELSE IF (TODAY - MovCliente.FecVenc) > 90 THEN 
                                DO:
                                    ASSIGN 
                                        l-totMas90 = l-totMas90 + l-saldo. /* Más de 90 días */
                                END.
                END.  
            END.    /*_ RNPC _*/
           
            IF MovCliente.Id-MC = 3 THEN 
            DO:
                ASSIGN 
                    l-numcheque = l-numcheque + 1.
                IF l-saldo = 0 THEN ASSIGN l-chequepag = l-chequepag + 1.
            END.
    
            /*_ RNPC - Obtengo el s�mbolo de la moneda _*/
            IF l-credito <> 0 THEN 
                l-RefValor = (MovCliente.Documento + (IF AVAILABLE Acuse AND Acuse.Estatus <> 4 THEN "&" ELSE '')).
            ELSE 
            DO:
                IF MovCliente.Id-Moneda > 1 THEN 
                DO:
                    FIND FIRST Moneda WHERE Moneda.Id-Moneda = MovCliente.Id-Moneda NO-LOCK NO-ERROR.
                    IF AVAILABLE Moneda THEN ASSIGN l-RefValor = Moneda.Simbolo.
                END.
                ELSE ASSIGN l-RefValor = "". 
            END.  
          

            /*IF USERID("dictdb") = "franc" THEN
               MESSAGE "3" VIEW-AS ALERT-BOX.*/
        
            FIND TabMC OF MovCliente NO-LOCK NO-ERROR.
            CREATE ttDetalle.
            ASSIGN 
                ttDetalle.IdCliente   = MovCliente.Id-Cliente
                ttDetalle.Documento   = MovCliente.RefSaldo
                ttDetalle.Fecha       = MovCliente.FecReg
                ttDetalle.Descripcion = IF AVAILABLE TabMc THEN TabMC.Descr ELSE ""
                ttDetalle.Cargo       = MovCliente.Importe 
                WHEN MovCliente.Importe > 0
                ttDetalle.Credito     = (MovCliente.Importe * -1) 
                WHEN MovCliente.Importe <= 0
                ttDetalle.Saldo       = 0.01
                ttDetalle.Margen      = ?
                ttDetalle.id-mc       = movcliente.id-mc
                ttDetalle.Antiguedad  = TODAY - l-fecha 
                ttDetalle.Referencia  = IF MovCliente.Id-NCR <> "" THEN
                                 MovCliente.Id-NCR ELSE (MovCliente.Documento + 
                                (IF AVAILABLE Acuse AND Acuse.Estatus <> 4 THEN "&" ELSE '')) 
                ttDetalle.Rec         = RECID(TabMC)     . 
               
            /* Buscar Acuse con base en la referencia que acabamos de construir */
            FIND FIRST Acuse 
                WHERE Acuse.Id-Acuse = ttDetalle.Referencia
                NO-LOCK NO-ERROR.

            /* Asignar TipoAcuse si se encontró el Acuse */
            IF AVAILABLE Acuse THEN
                ttDetalle.TipoAcuse = Acuse.Tipo.
            ELSE
                ttDetalle.TipoAcuse = "".      
                

            FIND Factura WHERE Factura.Id-Factura = MovCliente.RefSaldo NO-LOCK NO-ERROR.
            IF AVAILABLE Factura THEN 
            DO:
                IF MovCliente.Id-MC = 1 THEN 
                DO:
                    ASSIGN 
                        ttDetalle.PlazoFactura = Factura.Plazo
                        ttDetalle.Referencia   = Factura.Id-Fiscal.
                END.
                ELSE
                    ASSIGN ttDetalle.PlazoFactura = (MovCliente.FecReg - Factura.FecReg).
            END.
    
            IF MovCliente.Id-MC = 1 THEN 
            DO:
                FOR EACH Devolucion WHERE Devolucion.Id-Factura = MovCliente.RefSaldo AND
                    Devolucion.TipoVenta = 3 AND
                    Devolucion.VtaCanc    = TRUE NO-LOCK.
                    ASSIGN 
                        ttDetalle.Referencia = "D" + STRING(Devolucion.Id-Dev).
                END. /* del end de devolucion */
            END. /* del if movcliente.id-mc */
    
            /*_ RNPC - Obtengo Referencia _*/
            IF MovCliente.Id-MC = 1 AND (ttDetalle.Referencia = "" OR ttDetalle.Referencia = ttDetalle.Documento) THEN 
            DO:
                IF MovCliente.Id-Moneda > 1 THEN 
                DO:
                    FIND FIRST Moneda WHERE Moneda.Id-Moneda = MovCliente.Id-Moneda NO-LOCK NO-ERROR.
                    IF AVAILABLE Moneda THEN ASSIGN ttDetalle.Referencia = Moneda.simbolo.
                    l-HayMonedaEX = TRUE.
                END.
            END.
   
            /*_ RNPC - Por cada movimiento diferente _*/
            IF LAST-OF(MovCliente.RefSaldo) THEN 
            DO:            
                ASSIGN 
                    l-ant           = TODAY - l-fecha
                    l-saldo         = (ACCUM TOTAL BY MovCliente.RefSaldo l-cargo) +
                                      (ACCUM TOTAL BY MovCliente.RefSaldo l-credito)
                    l-saldoME       = l-cargoME + l-creditoME
                    ttDetalle.Saldo = l-saldo  
                    l-hubo          = TRUE.  

            
                IF l-saldoME > 0 THEN l-saldo = l-saldo + l-saldoME.    /*_ RNPC _*/
                ACCUMULATE l-saldo * l-ant (TOTAL).
            END.
            /*_ RNPC - Suma los totales de cr�dito y cargo al terminar de barrer los registros _*/
            IF LAST(b-mov.fecreg) AND LAST(MovCliente.REfSaldo) THEN 
            DO:
                ASSIGN 
                    l-saldo   = l-totcargo + l-totcredito
                    l-saldoME = l-totcargoME + l-totcreditoME.
            END.
            ASSIGN 
                l-linea2 = l-linea2 + 1.
     
     
       
        END. /* DEL for each MovCliente */
   
    END. /* del for each a b-mov*/
    FOR EACH ttDetalle :
        FIND RefPortal WHERE RefPortal.Id-Cliente = ttDetalle.IdCliente 
            AND RefPortal.Id-Factura = ttDetalle.Documento  NO-LOCK NO-ERROR.
       
        ASSIGN 
            ttDetalle.FolioEstatus = (IF AVAILABLE RefPortal THEN RefPortal.Estatus ELSE "")
            ttDetalle.Acuse        = (IF AVAILABLE RefPortal THEN RefPortal.Acuse ELSE "")
           
            ttDetalle.Registro     = (IF AVAILABLE RefPortal THEN RefPortal.FecReg ELSE ?).   
           
    END. 
    
    CREATE ttCartera.
    ASSIGN 
        ttCartera.id             = 1
        ttCartera.IdCliente      = lCliente
        ttCartera.saldo          = l-saldo
        ttcartera.porvencer      = l-pvencer
        ttCartera.montovencido   = (l-saldo - l-pvencer)   
        ttcartera.treinta        = l-tot30
        ttcartera.sesenta        = l-tot60
        ttcartera.noventa        = l-tot90
        ttcartera.noventamas     = l-totMas90
        ttcartera.margenpromedio = ?.  

    ASSIGN 
        l-acomodo = 10.
    FOR EACH bk-saldo WHERE bk-saldo.id-mc <= 3 NO-LOCK BY bk-saldo.Fecha :
        FIND ttDetalle WHERE RECID(ttDetalle) = RECID(bk-saldo) EXCLUSIVE NO-ERROR.
        ASSIGN 
            ttDetalle.Acomodo = l-acomodo 
            l-acomodo         = l-acomodo + 10.
        FOR EACH bbk-saldo WHERE bbk-saldo.Id-MC > 3 AND
            bbk-saldo.Documento = bk-saldo.Documento NO-LOCK
            BY bbk-saldo.Id-MC.
            FIND ttDetalle WHERE RECID(ttDetalle) = RECID(bbk-saldo) EXCLUSIVE NO-ERROR.
            ASSIGN 
                ttDetalle.acomodo = l-acomodo 
                l-acomodo         = l-acomodo + 10. 
        END. /* del bbk-saldo */
    END. /* del bk-saldo */   
    
    FOR EACH ttDetalle NO-LOCK:
        /* Si PlazoFactura es 0, lo ajustamos */
        IF ttDetalle.PlazoFactura = 0 THEN 
            ASSIGN ttDetalle.PlazoFactura = ?.

        /* Si Cargo es 0, lo ajustamos */
        IF ttDetalle.Cargo = 0 THEN 
            ASSIGN ttDetalle.Cargo = ?.

        /* Si Credito es 0, lo ajustamos */
        IF ttDetalle.Credito = 0 THEN 
            ASSIGN ttDetalle.Credito = ?.

        /* Si Saldo es menor o igual a 0.01, lo ajustamos */
        IF ttDetalle.Saldo = 0.01 THEN 
            ASSIGN ttDetalle.Saldo = ?.

        /* Si la Referencia es igual al Documento, ajustamos la Referencia */
        IF ttDetalle.Referencia = ttDetalle.Documento THEN 
            ASSIGN ttDetalle.Referencia = "".
        
        IF ttDetalle.Saldo <= 0.01 THEN
            ASSIGN ttDetalle.Antiguedad = ?.
    END.
    
    FOR EACH ttCartera  :
        DO TRANSACTION:                                                                             
            RUN cxcb0270.p(INPUT ttCartera.IdCliente,INPUT TODAY,OUTPUT l-dia-max ,OUTPUT l-prompago).
            RUN programas/margencte.p(INPUT ttCartera.IdCliente,OUTPUT v-margen).
            IF l-dia-max = ? THEN l-dia-max = 0.
        END.
        ASSIGN  
            ttCartera.diacartera     = l-dia-max
            ttCartera.promedio       = INTEGER(ROUND(l-prompago, 0)) /* Redondea a un entero */
            ttCartera.margenpromedio = v-margen .    
    END. 
     
    FOR EACH ttDetalle WHERE ttDetalle.Descripcion BEGINS "Fact":
        DO TRANSACTION:                                                                             
            RUN calcularMargenNeto(INPUT ttDetalle.Documento,OUTPUT  v-marneto).
        END.
        ASSIGN  
            ttDetalle.Margen = v-marneto. 
    END. 
          
END PROCEDURE.    


PROCEDURE calcularMargenNeto:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /*
       Programa: calcularMargenNeto.p
       Función : Calcular el margen bruto de una factura
       Autor   : [Tu nombre]
       Fecha   : [Fecha]
    */     


    DEFINE INPUT  PARAMETER pi-factura AS CHARACTER NO-UNDO.  /* Número de factura */
    DEFINE OUTPUT PARAMETER po-marventa AS DECIMAL   NO-UNDO  FORMAT "-zzz9.99%".  /* Margen bruto calculado */

    DEFINE VARIABLE l-tcosto   LIKE factura.subtotal NO-UNDO.
    DEFINE VARIABLE l-tprecio  LIKE factura.subtotal NO-UNDO.
    DEFINE VARIABLE l-pagos    LIKE factura.subtotal NO-UNDO.  
    DEFINE VARIABLE l-desc     LIKE factura.subtotal NO-UNDO.
    DEFINE VARIABLE l-dev      LIKE factura.subtotal NO-UNDO.
    DEFINE VARIABLE l-desinc   LIKE factura.subtotal NO-UNDO.
    DEFINE VARIABLE l-porcdesc AS DECIMAL NO-UNDO.
    DEFINE VARIABLE l-marventa AS DECIMAL NO-UNDO FORMAT "-zzz9.99%".
    DEFINE VARIABLE l-marneto  AS DECIMAL NO-UNDO.  

    /* Buscar la factura */
    FIND factura WHERE factura.id-factura = pi-factura NO-LOCK NO-ERROR.

    IF AVAILABLE factura THEN 
    DO:
        IF factura.feccanc = ? THEN 
        DO:
            /* Calcular costos y precios */
            l-tcosto = 0.
            l-tprecio = 0.

            FOR EACH detfactura OF factura NO-LOCK:
                l-tcosto = l-tcosto + (detfactura.costo * detfactura.cant).
                l-tprecio = l-tprecio + (detfactura.precunit * detfactura.cant * (1 - (detfactura.descto / 100))).
            END.

            /* Calcular pagos, descuentos y devoluciones */
            l-pagos = 0.
            l-desc = 0.
            l-dev = 0.

            FOR EACH movcliente WHERE movcliente.refsaldo = pi-factura NO-LOCK:
                IF (movcliente.id-mc >= 57 AND movcliente.id-mc <= 62) OR
                    (movcliente.id-mc = 90 OR movcliente.id-mc = 96) THEN
                    l-pagos = l-pagos + (movcliente.importe * -1).
                IF movcliente.id-mc = 63 OR movcliente.id-mc = 64 OR
                    movcliente.id-mc = 66 OR movcliente.id-mc = 68 THEN
                    l-desc = l-desc + (movcliente.importe * -1).
                IF movcliente.id-mc = 65 THEN
                    l-dev = l-dev + (movcliente.importe * -1).
            END.

            FOR EACH HistMovCte WHERE HistMovCte.refsaldo = pi-factura NO-LOCK:
                IF (HistMovCte.id-mc >= 57 AND HistMovCte.id-mc <= 62) OR
                    (HistMovCte.id-mc = 90 OR HistMovCte.id-mc = 96) THEN
                    l-pagos = l-pagos + (HistMovCte.importe * -1).
                IF HistMovCte.id-mc = 63 OR HistMovCte.id-mc = 64 OR
                    HistMovCte.id-mc = 66 OR HistMovCte.id-mc = 68 THEN
                    l-desc = l-desc + (HistMovCte.importe * -1).
                IF HistMovCte.id-mc = 65 THEN
                    l-dev = l-dev + (HistMovCte.importe * -1).
            END.

            /* Calcular margen neto */
            IF l-tcosto > 0 THEN
                l-marventa = (l-tprecio / l-tcosto - 1) * 100.
            ELSE
                l-marventa = 100.

            l-porcdesc = l-desc / (l-pagos + l-desc) * 100.
            l-marneto = ((l-tprecio * (1 - (l-porcdesc / 100))) / l-tcosto - 1) * 100.

            /* Asignar el valor de salida */
            po-marventa = l-marventa.
        END.  
        ELSE 
        DO:       
            MESSAGE "Factura Cancelada..." VIEW-AS ALERT-BOX TITLE "Error!".
            po-marventa = ?.  /* Retornar valor nulo si la factura está cancelada */
        END.
    END.  
    ELSE 
    DO:
        MESSAGE "Factura Inexistente" VIEW-AS ALERT-BOX TITLE "Error!".
        po-marventa = ?.  /* Retornar valor nulo si la factura no existe */
    END.      
     
  
END PROCEDURE.


