@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : anticipocons.p
    Purpose     : 

    Syntax      :/AnticipoCte

    Description : Consulta de anticipos por cliente

    Author(s)   : sis10
    Created     : Fri Jan 17 11:00:31 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttAnticipos NO-UNDO
    FIELD IdAnticipo  AS CHARACTER
    FIELD Fecha          AS DATE
    FIELD Acuse          AS CHARACTER
    FIELD ImporteAnticipo AS DECIMAL
    FIELD ImporteAplicado AS DECIMAL.


/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetConsultaAnticipo:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pCliente AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttAnticipos.


/* Limpiar la tabla temporal */
EMPTY TEMP-TABLE ttAnticipos.

/* Consulta de anticipos filtrando por cliente */
FOR EACH Anticipo
    WHERE Anticipo.Id-Cliente = pCliente
      AND Anticipo.ImpAnticipo > Anticipo.ImpAplicado
      AND Anticipo.Canc = FALSE     
    NO-LOCK:

    /* Crear un registro en la tabla temporal */
    CREATE ttAnticipos.
    ASSIGN
        ttAnticipos.IdAnticipo   = Anticipo.Id-Anticipo
        ttAnticipos.Fecha           = Anticipo.FecReg
        ttAnticipos.Acuse           = Anticipo.Id-Acuse
        ttAnticipos.ImporteAnticipo = Anticipo.ImpAnticipo
        ttAnticipos.ImporteAplicado = Anticipo.ImpAplicado.
END .     


END PROCEDURE.

