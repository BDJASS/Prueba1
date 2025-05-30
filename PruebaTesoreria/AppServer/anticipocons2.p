@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : anticipocons2.p
    Purpose     : 

    Syntax      :/Anticipo

    Description : Consulta de anticipos por nivel Anticipo y detalle

    Author(s)   : sis10
    Created     : Fri Jan 17 11:00:31 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttAnticipo NO-UNDO
    FIELD IdAnticipo  AS CHARACTER
    FIELD Fecha          AS DATE
    FIELD Acuse          AS CHARACTER
    FIELD ImporteAnticipo AS DECIMAL
    FIELD ImporteAplicado AS DECIMAL.
    
DEFINE TEMP-TABLE ttDetAnticipo NO-UNDO
    FIELD IdAnticipo  AS CHARACTER
    FIELD Sec         AS INT
    FIELD Documento   AS CHARACTER
    FIELD AplicadoPor    AS CHAR
    FIELD Importe     AS DECIMAL
    FIELD Fecha       AS DATE.   

DEFINE DATASET dsAnticipo FOR 
    ttAnticipo, /* Tabla principal */
    ttDetAnticipo /* Relación con IdAnticipo */
    DATA-RELATION AnticipoDetalle FOR ttAnticipo, ttDetAnticipo
        RELATION-FIELDS (IdAnticipo, IdAnticipo). /* Relación por IdAnticipo */ 
/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetConsultaAnticipo:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pAnticipo AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER DATASET FOR dsAnticipo.


/* Limpiar las tablas temporales */
    EMPTY TEMP-TABLE ttAnticipo.
    EMPTY TEMP-TABLE ttDetAnticipo.

    /* Buscar el anticipo */
    FIND FIRST Anticipo WHERE Anticipo.Id-Acuse = pAnticipo NO-LOCK NO-ERROR.

    /* Si el anticipo existe, llenamos la tabla ttAnticipo */
    IF AVAILABLE(Anticipo) THEN DO:
        CREATE ttAnticipo.
        ASSIGN
            ttAnticipo.IdAnticipo      = Anticipo.Id-Anticipo
            ttAnticipo.Fecha           = Anticipo.FecReg
            ttAnticipo.Acuse           = Anticipo.Id-Acuse
            ttAnticipo.ImporteAnticipo = Anticipo.ImpAnticipo
            ttAnticipo.ImporteAplicado = Anticipo.ImpAplicado.

        /* Buscar los detalles del anticipo */
        FOR EACH DetAnticipo WHERE DetAnticipo.Id-Anticipo = Anticipo.Id-Anticipo NO-LOCK:
            CREATE ttDetAnticipo.
            ASSIGN
                ttDetAnticipo.IdAnticipo = DetAnticipo.Id-Anticipo
                ttDetAnticipo.Sec        = DetAnticipo.Sec
                ttDetAnticipo.Documento  = DetAnticipo.Documento
                ttDetAnticipo.Importe    = DetAnticipo.Importe
                ttDetAnticipo.Fecha      = DetAnticipo.FecReg.
        END.
    END.

END PROCEDURE.


