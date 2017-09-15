CREATE OR REPLACE PACKAGE BODY CUSTOMER."ALZ_LAW_TICKET_PROCESS_UTILS" AS

PROCEDURE TMP_LOG (
    p_package_name         VARCHAR2,
    p_procedure_name       VARCHAR2,
    p_log_text             VARCHAR2,
    p_script_name          VARCHAR2 DEFAULT NULL,
    p_sequence             NUMBER DEFAULT -1)
IS
    PRAGMA AUTONOMOUS_TRANSACTION;

    v_loop_end           NUMBER;
    v_written_text       VARCHAR2(500);

    BEGIN

    IF p_log_text IS NOT NULL THEN
        v_loop_end:= LENGTH(p_log_text) / 500;

        FOR i in 0 .. v_loop_end LOOP
            v_written_text := null;

            IF LENGTH(TRIM(p_log_text)) > 500 THEN
                v_written_text := SUBSTR(p_log_text, i * 500 + 1, 500);
            ELSE
                v_written_text := p_log_text;
            END IF;


            IF LENGTH(TRIM(v_written_text)) > 0 THEN
                INSERT INTO tmp_koc_errors(REFERENCE_CODE, PROCESS, ERROR, PROCESS_DATE, SCRIPT_NAME, SIRA)
                     VALUES (p_package_name, p_procedure_name, v_written_text, SYSDATE, DECODE(p_script_name, NULL, p_package_name, p_script_name), DECODE(p_sequence, -1, i, p_sequence));
            END IF;

            COMMIT;
        END LOOP;
    END IF;

END;

PROCEDURE INSERT_TICKET_HISTORY (
      p_ticket_id     IN     VARCHAR2,
      p_subject       IN     VARCHAR2,
      p_user_name     IN     VARCHAR2
   )
   IS

   BEGIN

     INSERT INTO ALZ_LAW_TICKET_HISTORY (TICKET_ID, SUBJECT, CREATE_USER)
          VALUES (p_ticket_id, p_subject, p_user_name);

   END;


PROCEDURE GET_TICKET_HISTORY(p_ticket_id         IN          NUMBER,
                             cur                    OUT      refcur)
IS
BEGIN

    OPEN cur FOR
        SELECT TICKET_ID, SUBJECT, CREATE_USER, CREATE_TIME
              FROM ALZ_LAW_TICKET_HISTORY
             WHERE TICKET_ID = p_ticket_id
          ORDER BY CREATE_TIME DESC;
END;

PROCEDURE GET_GROUPS(cur   OUT     refcur)
IS
BEGIN

    OPEN cur FOR
        SELECT A.CODE,
               A.PARENT,
               A.DEFINITION,
               A.VALIDITY_START_DATE,
               A.VALIDITY_END_DATE,
               A.ASSIGNABLE
          FROM ALZ_LAW_GROUP_DEF A
         WHERE VALIDITY_START_DATE <= TRUNC(SYSDATE)
           AND (VALIDITY_END_DATE >= TRUNC(SYSDATE) OR VALIDITY_END_DATE IS NULL)
      ORDER BY CODE;
END;

PROCEDURE GET_GROUP_USERS(p_group_code        IN         VARCHAR2,
                          cur                OUT         refcur)
IS
BEGIN

    OPEN cur FOR
        SELECT A.USERNAME,
               A.GROUP_CODE,
               NVL(B.STATUS, 'UYGUN') STATUS,
               A.VALIDITY_START_DATE,
               A.VALIDITY_END_DATE,
               CASE WHEN C.SURNAME IS NULL THEN C.NAME
               ELSE C.FIRST_NAME || ' ' || C.SURNAME
               END DISPLAY_NAME
          FROM ALZ_LAW_GROUP_USER_REL A, ALZ_LAW_TICKET_USER_STATUS B,WEB_SEC_SYSTEM_USERS WSS, CP_PARTNERS C
         WHERE A.USERNAME = B.USER_ID(+)
           AND A.GROUP_CODE = p_group_code
           AND A.USERNAME = WSS.USER_NAME
           AND WSS.CUSTOMER_PARTNER_ID = C.PART_ID(+)
           AND A.VALIDITY_START_DATE <= TRUNC(SYSDATE)
           AND (A.VALIDITY_END_DATE >= TRUNC(SYSDATE) OR A.VALIDITY_END_DATE IS NULL)
           AND B.VALIDITY_START_DATE(+) <= SYSDATE
           AND (B.VALIDITY_END_DATE(+) IS NULL OR B.VALIDITY_END_DATE(+) > SYSDATE)
           order by display_name , username;
END;

PROCEDURE INSERT_TICKET_NOTE(p_ticket_id         IN         NUMBER,
                             p_note              IN         CLOB,
                             p_is_internal       IN         NUMBER,
                             p_create_user       IN         VARCHAR2,
                             p_process_results  OUT         customer.process_result_table)
IS
BEGIN

    INSERT INTO ALZ_LAW_TICKET_NOTE (ID,TICKET_ID,NOTE,INTERNAL,CREATE_USER,CREATE_DATE)
    VALUES (ALZ_LAW_TICKET_DETAIL_SEQ.NEXTVAL, p_ticket_id, p_note, DECODE(p_is_internal, 1, 0, 1), p_create_user, SYSDATE);

    ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_TICKET_HISTORY(p_ticket_id, 'Not eklendi.', p_create_user);

EXCEPTION WHEN OTHERS THEN
    alz_web_process_utils.process_result (
        0,
        9,
        -1,
       'ORACLE_EXCEPTION',
        SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
        null,
        null,
        null,
        'ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_TICKET_NOTE',
        NULL,
        p_process_results);

END;


PROCEDURE GET_TICKET_NOTES(p_ticket_id         IN      NUMBER,
                           p_user_name         IN      VARCHAR2,
                           cur                OUT      refcur)
IS
    v_lmha NUMBER;
    v_lmmh NUMBER;

BEGIN

    v_lmha := KOC_AUTH_UTILS.is_user_authorized_rn(p_user_name, 'LMRHA');

    v_lmmh := KOC_AUTH_UTILS.is_user_authorized_rn(p_user_name, 'LMRMH');

    OPEN cur FOR
        SELECT A.CREATE_DATE,
               A.CREATE_USER,
               A.NOTE,
               A.TICKET_ID
          FROM ALZ_LAW_TICKET_NOTE A
         WHERE A.TICKET_ID = p_ticket_id
           AND (((v_lmha = 1 OR v_lmmh = 1) AND INTERNAL = 0) OR (v_lmha = 0 AND v_lmmh = 0) )
      ORDER BY CREATE_DATE DESC;
END;

PROCEDURE GET_SAFAHAT_LIST(p_uyap_id           IN      NUMBER,
                           p_company_name      IN      VARCHAR2,
                           cur                OUT      refcur)
IS
BEGIN

    OPEN cur FOR
        SELECT A.ACIKLAMA,
               A.ISLEM_TARIHI_DATE,
               A.ISLEM_YAPAN_PERSONEL,
               B.SAFAHAT_TUR_ACIKLAMA
        FROM ALZ_UYAP_SAFAHAT A, ALZ_UYAP_SAFAHAT_TURLERI B
        WHERE A.DOSYA_ID = p_uyap_id
              AND A.SEFAHAT_TURU_KOD = B.SAFAHAT_TUR_KODU
              AND A.GUNCEL_VERSIYON = 'Y'
              AND A.SIRKET_ADI = p_company_name
              ORDER BY A.ISLEM_TARIHI DESC;
END;

PROCEDURE INSERT_THIRD_PARTY_DOC (p_ticket_id            IN NUMBER,
                                  p_filenet_id           IN VARCHAR2,
                                  p_user_name            IN VARCHAR2,
                                  p_group                IN VARCHAR2,
                                  p_process_results      OUT customer.process_result_table)
    IS

       v_result  NUMBER;

    CURSOR c_ticket_id
    IS
        SELECT 1
          FROM alz_third_party_documents a
         WHERE A.TICKET_ID = p_ticket_id
          AND A.FILENET_ID = p_filenet_id;


    BEGIN

        OPEN c_ticket_id;
        FETCH c_ticket_id INTO v_result;
        CLOSE c_ticket_id;

        IF nvl(v_result,0) = 0 THEN

                INSERT INTO alz_third_party_documents (TICKET_ID,FILENET_ID,USERNAME,CREATE_DATE,GROUP_CODE)
                VALUES (p_ticket_id,p_filenet_id,p_user_name,SYSDATE,p_group);

        ELSE

                UPDATE alz_third_party_documents a
                   SET A.FILENET_ID = p_filenet_id,
                       A.GROUP_CODE = p_group,
                       A.CREATE_DATE = SYSDATE,
                       A.USERNAME = p_user_name
                 WHERE A.TICKET_ID = p_ticket_id
                   AND A.FILENET_ID = p_filenet_id;

        END IF;

        ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_TICKET_HISTORY(p_ticket_id, p_group || ' grubu kullanýcýlarýna evrak görme yetkisi verildi.', p_user_name);

EXCEPTION
    WHEN OTHERS THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
           'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
            'ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_THIRD_PARTY_DOC',
            NULL,
            p_process_results);

    END;


PROCEDURE GET_SUPERIOR_EMAIL_BY_USER (p_user_name         IN          VARCHAR2,
                                      cur                     OUT     refcur,
                                      p_process_results       OUT     customer.process_result_table)
IS
    v_result    refcur;

    v_exists  NUMBER;

    CURSOR C1 IS
    SELECT 1
          FROM WEB_SEC_SYSTEM_USERS A, CP_PARTNERS B
         WHERE A.CUSTOMER_PARTNER_ID = B.PART_ID
           AND EXISTS (SELECT 1
                         FROM ALZ_LAW_GROUP_USER_REL C
                        WHERE C.USERNAME = A.USER_NAME
                          AND EXISTS (SELECT 1
                                        FROM ALZ_LAW_GROUP_DEF D, ALZ_LAW_GROUP_USER_REL E
                                       WHERE D.PARENT = C.GROUP_CODE
                                         AND E.USERNAME = p_user_name
                                         AND D.CODE = E.GROUP_CODE
                                         AND D.VALIDITY_START_DATE <= TRUNC(SYSDATE)
                                         AND (D.VALIDITY_END_DATE IS NULL
                                                OR D.VALIDITY_END_DATE >= TRUNC(SYSDATE))
                                         AND E.VALIDITY_START_DATE <= TRUNC(SYSDATE)
                                         AND (E.VALIDITY_END_DATE IS NULL
                                                OR E.VALIDITY_END_DATE >= TRUNC(SYSDATE))
                                         ))
           AND B.EMAIL LIKE '%@%.%';

BEGIN

      OPEN C1;
     FETCH C1
      INTO v_exists;
     CLOSE C1;


    IF v_exists IS NULL THEN

        OPEN v_result for
            SELECT B.EMAIL,
                   A.USER_NAME
              FROM WEB_SEC_SYSTEM_USERS A, CP_PARTNERS B
             WHERE A.CUSTOMER_PARTNER_ID = B.PART_ID
               AND EXISTS (SELECT 1
                             FROM ALZ_LAW_GROUP_USER_REL C
                            WHERE C.USERNAME = A.USER_NAME
                              AND EXISTS (SELECT 1
                                            FROM ALZ_LAW_GROUP_DEF D
                                           WHERE c.group_code = d.parent
                                             AND EXISTS (SELECT 1
                                                           FROM ALZ_LAW_GROUP_DEF K, ALZ_LAW_GROUP_USER_REL L
                                                          WHERE K.PARENT = d.code
                                                            AND L.USERNAME = p_user_name
                                                            AND K.CODE = L.GROUP_CODE
                                                            AND K.VALIDITY_START_DATE <= TRUNC(SYSDATE)
                                                            AND (K.VALIDITY_END_DATE IS NULL
                                                                   OR K.VALIDITY_END_DATE >= TRUNC(SYSDATE))
                                                            AND L.VALIDITY_START_DATE <= TRUNC(SYSDATE)
                                                            AND (L.VALIDITY_END_DATE IS NULL
                                                                   OR L.VALIDITY_END_DATE >= TRUNC(SYSDATE))
                                             )))
               AND B.EMAIL LIKE '%@%.%';
    ELSE

        OPEN v_result FOR
            SELECT B.EMAIL,
                   A.USER_NAME
              FROM WEB_SEC_SYSTEM_USERS A, CP_PARTNERS B
             WHERE A.CUSTOMER_PARTNER_ID = B.PART_ID
               AND EXISTS (SELECT 1
                             FROM ALZ_LAW_GROUP_USER_REL C
                            WHERE C.USERNAME = A.USER_NAME
                              AND EXISTS (SELECT 1
                                            FROM ALZ_LAW_GROUP_DEF D, ALZ_LAW_GROUP_USER_REL E
                                           WHERE D.PARENT = C.GROUP_CODE
                                             AND E.USERNAME = p_user_name
                                             AND D.CODE = E.GROUP_CODE
                                             AND D.VALIDITY_START_DATE <= TRUNC(SYSDATE)
                                             AND (D.VALIDITY_END_DATE IS NULL
                                                    OR D.VALIDITY_END_DATE >= TRUNC(SYSDATE))
                                             AND E.VALIDITY_START_DATE <= TRUNC(SYSDATE)
                                             AND (E.VALIDITY_END_DATE IS NULL
                                                    OR E.VALIDITY_END_DATE >= TRUNC(SYSDATE))
                                             ))
               AND B.EMAIL LIKE '%@%.%';

    END IF;

    cur := v_result;


EXCEPTION WHEN OTHERS THEN
    alz_web_process_utils.process_result (
        0,
        9,
        -1,
       'ORACLE_EXCEPTION',
        SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
        null,
        null,
        null,
        'ALZ_LAW_TICKET_PROCESS_UTILS.GET_SUPERIOR_EMAIL_BY_USER',
        NULL,
        p_process_results);

END;

PROCEDURE GET_EMAIL_BY_USER (p_user_name         IN      VARCHAR2,
                             cur                 OUT     refcur
                             )

IS
BEGIN

    OPEN cur FOR
        SELECT B.EMAIL,A.USER_NAME
          FROM WEB_SEC_SYSTEM_USERS A, CP_PARTNERS B
         WHERE A.CUSTOMER_PARTNER_ID = B.PART_ID AND A.USER_NAME = p_user_name;
END;

PROCEDURE INSERT_DOCUMENT_CONTENTS(p_uyap_id                  IN         NUMBER,
                                   p_document_id              IN         NUMBER,
                                   p_document_serial_no       IN         NUMBER,
                                   p_add_document             IN         VARCHAR2,
                                   p_filenet_id               IN         VARCHAR2,
                                   p_mime_type                IN         VARCHAR2,
                                   p_company_name             IN         VARCHAR2,
                                   p_process_results            OUT     customer.process_result_table
                                  )

IS

    BEGIN

    INSERT INTO alz_uyap_evrak_icerik (DOSYA_ID,EVRAK_ID,EVRAK_SIRA_NO,EK_EVRAK,FILENET_ID,KAYIT_TARIHI,MIMETYPE,SIRKET_ADI)
    VALUES (p_uyap_id,p_document_id,p_document_serial_no,p_add_document,p_filenet_id,SYSDATE,p_mime_type,p_company_name);

EXCEPTION WHEN OTHERS THEN
    alz_web_process_utils.process_result (
        0,
        9,
        -1,
       'ORACLE_EXCEPTION',
        SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
        null,
        null,
        null,
        'ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_DOCUMENT_CONTENTS',
        NULL,
        p_process_results);

    END;

FUNCTION UYAP_FILE_STATUS(uyap_file_id NUMBER,company_name VARCHAR2) RETURN VARCHAR2 IS
v_desc VARCHAR2(100);
BEGIN
  SELECT DOSYA_DURUMU
    INTO v_desc
    FROM ALZ_UYAP_DOSYA_LISTESI
   WHERE DOSYA_ID = uyap_file_id
     AND SIRKET_ADI = company_name
     AND GUNCEL_VERSIYON = 'Y';
  RETURN v_desc;
  EXCEPTION WHEN OTHERS THEN
    RETURN NULL;
END;

PROCEDURE GET_TICKET_LIST(p_ticket_id                        IN          NUMBER,
                          p_law_file_no                      IN          VARCHAR2,
                          p_uyap_id                          IN          NUMBER,
                          p_lawyer_part_id                   IN          NUMBER,
                          p_in_lawyer                        IN          VARCHAR2,
                          p_contact_person                   IN          VARCHAR2,
                          p_contact_person_id                IN          NUMBER,
                          p_file_open_date_start             IN          VARCHAR2,
                          p_file_open_date_end               IN          VARCHAR2,
                          p_ticket_close_date_start          IN          VARCHAR2,
                          p_ticket_close_date_end            IN          VARCHAR2,
                          p_opus_file_type                   IN          VARCHAR2,
                          p_uyap_file_type                   IN          VARCHAR2,
                          p_court_enf_no                     IN          VARCHAR2,
                          p_court_enf_type                   IN          VARCHAR2,
                          p_court_file_info                  IN          VARCHAR2,
                          p_receive_date_start               IN          VARCHAR2,
                          p_receive_date_end                 IN          VARCHAR2,
                          p_status                           IN          VARCHAR2,
                          p_parent_id                        IN          NUMBER,
                          p_ticket_type                      IN          NUMBER,
                          p_source                           IN          VARCHAR2,
                          p_doc_type_code                    IN          NUMBER,
                          p_create_date_start                IN          VARCHAR2,
                          p_create_date_end                  IN          VARCHAR2,
                          p_folder_no                        IN          VARCHAR2,
                          p_barcode_no                       IN          VARCHAR2,
                          p_with_sub_tickets                 IN          NUMBER,
                          p_lawyer_reference                 IN          VARCHAR2,
                          p_user_name                        IN          VARCHAR2,
                          cur                                    OUT     refcur,
                          p_process_results                      OUT     customer.process_result_table)

IS

    v_dynamic_sql       VARCHAR2(20000);
    user_group_code     VARCHAR2(10);

    CURSOR c_user_group_name(p_user_name_ VARCHAR2)
    IS
        SELECT GROUP_CODE
          FROM ALZ_LAW_GROUP_USER_REL
         WHERE VALIDITY_START_DATE <= SYSDATE
           AND (VALIDITY_END_DATE IS NULL OR VALIDITY_END_DATE >=SYSDATE)
           AND USERNAME = p_user_name_;

    CURSOR c_user_group_list(p_user_group_name_ VARCHAR2)
    IS
        SELECT LEVEL,
               lpad('',(level)) || to_char(CODE) TREE
          FROM ALZ_LAW_GROUP_DEF
         START WITH PARENT IN (p_user_group_name_)
       CONNECT BY PRIOR CODE = PARENT;

    reco_user_group          c_user_group_list%ROWTYPE;

BEGIN
    v_dynamic_sql:= 'SELECT A.LAWFILE_NO, '
                ||  '       A.RUCU_FILE_NO, '
                ||  '       A.UYAP_DOSYA_ID, '
                ||  '       CASE WHEN E.SURNAME IS NULL THEN E.NAME ELSE E.FIRST_NAME || '' ''  || E.SURNAME END LAWYER, '
                ||  '       H.NAME_SURNAME IN_LAWYER, '
                ||  '       F.EXPLANATION FILE_TYPE, '
                ||  '       I.COURT_NAME, '
                ||  '       A.LAW_COURT_ENF, '
                ||  '       A.UYAP_BIRIM_ID, '
                ||  '       A.COURT_FILE_NO, '
                ||  '       A.SOURCE, '
                ||  '       A.SUBJECT, '
                ||  '       B.STATUS, '
                ||  '       D.FILE_OPEN_DATE, '
                ||  '       B.RECEIVE_DATE, '
                ||  '       B.ARRIVAL_DATE, '
                ||  '       B.DEADLINE_DATE, '
                ||  '       B.CLOSE_DATE, '
                ||  '       B.MUHABERAT_DEADLINE_DATE, '
                ||  '       B.TICKET_TYPE_ID, '
                ||  '       G.DESCRIPTION TICKET_TYPE, '
                ||  '       A.TICKET_ID, '
                ||  '       A.CATEGORY, '
                ||  '       A.PARENT, '
                ||  '       A.FILENET_ID, '
                ||  '       A.DOC_RECEIVED, '
                ||  '       B.OWNER, '
                ||  '       B.OWNER_GROUP, '
                ||  '       B.DEPARTMENT, '
                ||  '       A.SIRKET_ADI, '
                ||  '       NVL(P.PRIORITY_LEVEL, 0) PRIORITY_LEVEL, '
                ||  '       NVL(C.DOC_DESC , (SELECT TURU FROM ALZ_UYAP_EVRAKLAR WHERE UNIQUE_ID=A.UYAP_EVRAK_UNIQUE_ID AND ROWNUM=1)) DOC_DESC, '
                ||  '       L.BARCODE_NO, '
                ||  '       L.FOLDER_NO, '
                ||  '       DECODE(A.LAWFILE_NO, NULL, DECODE(A.UYAP_DOSYA_ID, NULL, NULL, ALZ_LAW_TICKET_PROCESS_UTILS.UYAP_FILE_STATUS(A.UYAP_DOSYA_ID, A.SIRKET_ADI)), KOC_LAW_UTILS.STATUS_DESCRIPTION(D.STATUS_ID, ''G'')) FILE_STATUS, '
                ||  '       ALZ_LAW_TICKET_PROCESS_UTILS.GET_TIME_PASS(B.ARRIVAL_DATE, B.MUHABERAT_DEADLINE_DATE, B.CLOSE_DATE) MUHABERAT_TIME_PASS, '
                ||  '       ALZ_LAW_TICKET_PROCESS_UTILS.GET_TIME_LEFT(B.RECEIVE_DATE, B.MUHABERAT_DEADLINE_DATE, B.CLOSE_DATE) MUHABERAT_TIME_LEFT, '
                ||  '       ALZ_LAW_TICKET_PROCESS_UTILS.GET_TIME_PASS(B.ARRIVAL_DATE, B.DEADLINE_DATE, B.CLOSE_DATE) TIME_PASS, '
                ||  '       ALZ_LAW_TICKET_PROCESS_UTILS.GET_TIME_LEFT(B.RECEIVE_DATE, B.DEADLINE_DATE, B.CLOSE_DATE) TIME_LEFT, '
                ||  '       B.OLD_OWNER, '
                ||  '       B.OLD_OWNER_GROUP, '
                ||  '       B.REDIRECTOR, '
                ||  '       B.REDIRECTION_REASON_CODE, '
                ||  '       D.LAWYER_REFERENCE '
                ||  '  FROM ALZ_LAW_TICKET A, '
                ||  '       ALZ_LAW_TICKET_PROCESS B, '
                ||  '       ALZ_LAW_DOC_TYPE_DEF C, '
                ||  '       KOC_LAW_BASES D, '
                ||  '       CP_PARTNERS E, '
                ||  '       KOC_LAW_SF_TYPE F, '
                ||  '       ALZ_LAW_TICKET_TYPE G, '
                ||  '       ALZ_LAWYERS H, '
                ||  '       ALZ_LAW_COURTS_V I, '
                ||  '       ALZ_LAW_TICKET_USER_PRIORITY P, '
                ||  '       ALZ_CLM_COM_DOCS_INDEX_TBL J, '
                ||  '       ALZ_LAW_COM_TBL L, '
                ||  '       ALZ_LAW_GROUP_DEF M '
                ||  ' WHERE A.TICKET_ID = B.TICKET_ID '
                ||  '   AND A.CODE = C.CODE (+) '
                ||  '   AND A.LAWFILE_NO = D.LAW_FILE_NO (+) '
                ||  '   AND D.LAWYER = E.PART_ID (+) '
                ||  '   AND D.LAW_SF_TYPE = F.LAW_SF_TYPE (+) '
                ||  '   AND B.TICKET_TYPE_ID = G.TICKET_TYPE_ID '
                ||  '   AND D.IN_LAWYER = H.ID(+) '
                ||  '   AND A.UYAP_BIRIM_ID = I.COURT_ID(+) '
                ||  '   AND A.LAW_COURT_ENF = I.COURT_TYPE(+) '
                ||  '   AND A.TICKET_ID = P.TICKET_ID (+) '
                ||  '   AND A.FILENET_ID = J.FILENET_ID (+) '
                ||  '   AND J.ARCHIVE_NO = L.ARCHIVE_NO (+) '
                ||  '   AND J.COMMUNICATION_NO = L.COMMUNICATION_NO (+) '
                ||  '   AND B.OWNER_GROUP = M.CODE '
                ||  '   AND P.USER_NAME (+)= ''' || p_user_name || ''' '
                ||  '   AND B.STATUS <> ''CANCELLED'' ';


    IF NVL(p_ticket_type, 0) = 0 THEN   /* bildirimler 1 gonderiyor iþler 1 disindakiler*/
        v_dynamic_sql := v_dynamic_sql || ' AND G.TICKET_TYPE_ID <> 1 ';
    ELSE
        v_dynamic_sql := v_dynamic_sql || ' AND G.TICKET_TYPE_ID = ' || p_ticket_type || ' ';
    END IF;

    IF NVL(p_with_sub_tickets, 0) = 0 and NVL(p_parent_id, 0) = 0 THEN
        v_dynamic_sql := v_dynamic_sql || ' AND B.STATUS NOT IN (''PAIRED'', ''UNIFIED'') ';
    END IF;

    IF NVL(p_ticket_id, 0) != 0 THEN
        v_dynamic_sql := v_dynamic_sql || ' AND  A.TICKET_ID = ' || p_ticket_id || ' ';
    END IF;

    IF NVL(p_parent_id, 0) != 0 THEN
        v_dynamic_sql := v_dynamic_sql || ' AND (A.PARENT = ' || p_parent_id || ' OR A.TICKET_ID = ' || p_parent_id || ' )';
    END IF;

    IF p_law_file_no IS NOT NULL THEN
        v_dynamic_sql := v_dynamic_sql || ' AND  (A.LAWFILE_NO like ''%' || p_law_file_no || ''' ';
        v_dynamic_sql := v_dynamic_sql || ' OR  A.RUCU_FILE_NO like ''%' || p_law_file_no || ''' )';
    END IF;

    IF p_folder_no IS NOT NULL THEN
        v_dynamic_sql := v_dynamic_sql || ' AND  L.FOLDER_NO = ''' || p_folder_no || ''' ';
    END IF;

    IF p_barcode_no IS NOT NULL THEN
        v_dynamic_sql := v_dynamic_sql || ' AND  L.BARCODE_NO = ''' || p_barcode_no || ''' ';
    END IF;

    IF NVL(p_uyap_id, 0) != 0 THEN
        v_dynamic_sql := v_dynamic_sql || ' AND  A.UYAP_DOSYA_ID = ' || p_uyap_id || ' ';
    END IF;

    IF NVL(p_lawyer_part_id, 0) != 0 THEN
        v_dynamic_sql := v_dynamic_sql || ' AND  D.LAWYER = ' || p_lawyer_part_id || ' ';
    END IF;

    IF p_lawyer_reference IS NOT NULL THEN
        v_dynamic_sql := v_dynamic_sql || ' AND  LOWER(D.LAWYER_REFERENCE) like ''%' || LOWER(p_lawyer_reference) || '%'' ';
    END IF;

    IF p_in_lawyer IS NOT NULL THEN
        v_dynamic_sql := v_dynamic_sql || ' AND  D.IN_LAWYER =  ' || p_in_lawyer || ' ';
    END IF;

    IF NVL(p_contact_person_id, 0) != 0 THEN
        v_dynamic_sql := v_dynamic_sql || ' AND  (B.OWNER =  ''' || p_contact_person || ''' ';
        v_dynamic_sql := v_dynamic_sql || '       OR  B.OWNER =  (select USER_NAME from web_sec_system_users where CUSTOMER_PARTNER_ID = ''' || p_contact_person_id || ''')) ';
    END IF;

    IF p_file_open_date_start IS NOT NULL AND p_file_open_date_end IS NOT NULL THEN
        v_dynamic_sql := v_dynamic_sql || ' AND  TRUNC(D.FILE_OPEN_DATE) BETWEEN TO_DATE(''' || p_file_open_date_start ||''',''dd/MM/yyyy'')' || ' AND TO_DATE(''' || p_file_open_date_end  || ''',''dd/MM/yyyy'')';
    END IF;

    IF p_ticket_close_date_start IS NOT NULL AND p_ticket_close_date_end IS NOT NULL THEN
        v_dynamic_sql := v_dynamic_sql || ' AND  TRUNC(B.CLOSE_DATE) BETWEEN TO_DATE(''' || p_ticket_close_date_start ||''',''dd/MM/yyyy'')' || ' AND TO_DATE(''' || p_ticket_close_date_end  || ''',''dd/MM/yyyy'')';
    END IF;

    IF p_opus_file_type IS NOT NULL THEN
        v_dynamic_sql := v_dynamic_sql || ' AND  F.LAW_SF_TYPE =  ''' || p_opus_file_type || ''' ';
    END IF;

    IF p_court_enf_no IS NOT NULL AND p_court_enf_type IS NOT NULL THEN
        v_dynamic_sql := v_dynamic_sql || ' AND  A.LAW_COURT_ENF = ''' || p_court_enf_type || ''' AND A.UYAP_BIRIM_ID = ''' || p_court_enf_no || ''' ';
    END IF;

    IF p_court_file_info IS NOT NULL THEN
        v_dynamic_sql := v_dynamic_sql || ' AND  A.COURT_FILE_NO = ''' || p_court_file_info || ''' ';
    END IF;

    IF p_receive_date_start IS NOT NULL AND p_receive_date_end IS NOT NULL THEN
        v_dynamic_sql := v_dynamic_sql || ' AND  TRUNC(B.RECEIVE_DATE) BETWEEN TO_DATE(''' || p_receive_date_start ||''',''dd/MM/yyyy'')' || ' AND TO_DATE(''' || p_receive_date_end  || ''',''dd/MM/yyyy'')';
    END IF;

    IF p_create_date_start IS NOT NULL AND p_create_date_end IS NOT NULL THEN
        v_dynamic_sql := v_dynamic_sql || ' AND  TRUNC(B.ARRIVAL_DATE) BETWEEN TO_DATE(''' || p_create_date_start ||''',''dd/MM/yyyy'')' || ' AND TO_DATE(''' || p_create_date_end  || ''',''dd/MM/yyyy'')';
    END IF;

    IF p_status IS NOT NULL AND p_status <> 'REDIRECTED' THEN     /* Islerin durumu, open, closed, read, unread */
        v_dynamic_sql := v_dynamic_sql || ' AND  ((B.STATUS = ''' || p_status || ''' ';
        v_dynamic_sql := v_dynamic_sql || ' OR  B.STATUS = ''PAIRED'' ';
        v_dynamic_sql := v_dynamic_sql || ' OR  B.STATUS = ''UNIFIED'') ';

        IF NVL(p_with_sub_tickets, 0) <> 0 THEN
            v_dynamic_sql := v_dynamic_sql || ' AND  (( ( B.STATUS = ''' || p_status || ''' ) AND A.PARENT IS NULL ) ';
            v_dynamic_sql := v_dynamic_sql || ' OR ( A.PARENT IS NOT NULL AND EXISTS (SELECT 1 FROM ALZ_LAW_TICKET_PROCESS Z WHERE Z.TICKET_ID = A.PARENT AND Z.STATUS=''' || p_status || ''' )';
            v_dynamic_sql := v_dynamic_sql || ' AND (  B.STATUS = ''PAIRED''  OR  B.STATUS = ''UNIFIED'') ))';
        END IF;


--        IF NVL(p_with_sub_tickets, 0) <> 0 THEN
--            v_dynamic_sql := v_dynamic_sql || 'AND (A.PARENT IS NOT NULL AND EXISTS';
--            v_dynamic_sql := v_dynamic_sql || '(SELECT 1 FROM ALZ_LAW_TICKET_PROCESS Z WHERE Z.TICKET_ID = A.PARENT';
--            v_dynamic_sql := v_dynamic_sql || ' AND Z.STATUS = '''|| 'p_status' ||''')';
--            v_dynamic_sql := v_dynamic_sql || ' OR (A.PARENT IS NULL))';
--        END IF;

--        IF NVL(p_with_sub_tickets, 0) <> 0 THEN
--            v_dynamic_sql := v_dynamic_sql || ' AND EXISTS (SELECT 1 FROM ALZ_LAW_TICKET_PROCESS Z WHERE Z.TICKET_ID = A.PARENT AND Z.STATUS = '''|| p_status ||''') ';
--        END IF;

        v_dynamic_sql := v_dynamic_sql || ') ';
    END IF;

    IF p_source IS NOT NULL THEN
        v_dynamic_sql := v_dynamic_sql || ' AND  A.SOURCE = ''' || p_source || ''' ';
    END IF;

    IF NVL(p_doc_type_code, 0) > 0 THEN
        v_dynamic_sql := v_dynamic_sql || ' AND  A.CODE = ' || p_doc_type_code || ' ';
    END IF;

    IF p_status IS NOT NULL AND p_status = 'REDIRECTED' THEN
        v_dynamic_sql := v_dynamic_sql || ' AND  (B.REDIRECTOR = ''' || p_user_name || ''' OR B.OLD_OWNER = ''' || p_user_name || ''' ) ';
    ELSE
         OPEN c_user_group_name (p_user_name);
        FETCH c_user_group_name INTO user_group_code;
        CLOSE c_user_group_name;

        IF NVL(p_contact_person_id, 0) <> -1 THEN
            v_dynamic_sql := v_dynamic_sql || ' AND ( ';
            v_dynamic_sql := v_dynamic_sql || ' B.OWNER = ''' || p_user_name || ''' ';

            OPEN c_user_group_list (user_group_code);
            LOOP
                 FETCH c_user_group_list INTO reco_user_group;
                 EXIT WHEN c_user_group_list%NOTFOUND;
                   v_dynamic_sql := v_dynamic_sql || ' OR  B.OWNER_GROUP = ''' || reco_user_group.TREE || ''' ';

            END LOOP;
            CLOSE c_user_group_list;

            v_dynamic_sql := v_dynamic_sql || ' OR (EXISTS
                 (
                 SELECT 1
                   FROM ALZ_LAW_GROUP_USER_REL A,
                        ALZ_LAW_GROUP_DEF B
                  WHERE A.USERNAME = ''' || p_user_name || '''
                    AND A.GROUP_CODE = B.CODE
                    AND B.IS_EXT_MANAGER = 1
                 )
                 AND B.OLD_OWNER_GROUP IN
                 (
                    select code
                      from alz_law_group_def
                     start with code in
                     (
                         select group_code
                          from alz_law_group_user_rel
                         where validity_start_date <= sysdate
                           and (validity_end_date is null or validity_end_date >=sysdate)
                           and username = ''' || p_user_name || '''
                     )
                   connect by prior code = parent
                 ) AND M.IS_EXTERNAL = 1)) ';


--            v_dynamic_sql := v_dynamic_sql || ' OR ((B.OWNER_GROUP = ''LMHA'' OR B.OWNER_GROUP = ''LMMUNAUZ'' OR B.OWNER_GROUP = ''LMTPAD'' OR B.OWNER_GROUP = ''LMTPSE'') AND ( (B.OLD_OWNER = ''' || p_user_name || ''' AND ''' || user_group_code || ''' NOT IN (''LMHA'', ''LMMUNAUZ'', ''LMTPAD'', ''LMTPSE''))';

--            OPEN c_user_group_list (user_group_code);
--            LOOP
--                 FETCH c_user_group_list INTO reco_user_group;
--                 EXIT WHEN c_user_group_list%NOTFOUND;
--                   v_dynamic_sql := v_dynamic_sql || ' OR  B.OLD_OWNER_GROUP = ''' || reco_user_group.TREE || ''' ';

--            END LOOP;
--            CLOSE c_user_group_list;

--            v_dynamic_sql := v_dynamic_sql || ' ) ) )';
        ELSE
            IF user_group_code = 'LMHA' OR user_group_code = 'LMMUNAUZ' OR user_group_code = 'LMTPAD' OR user_group_code = 'LMTPSE' THEN
                v_dynamic_sql := v_dynamic_sql || ' AND B.OWNER = ''' || p_user_name || ''' ';
            END IF;
        END IF;
    END IF;


    IF NVL(p_ticket_type, 0) = 1 THEN
        v_dynamic_sql := v_dynamic_sql || ' ORDER BY B.RECEIVE_DATE';
    ELSIF NVL(p_parent_id, 0) != 0 THEN
        v_dynamic_sql := v_dynamic_sql || ' ORDER BY A.CATEGORY';
    ELSIF KOC_AUTH_UTILS.IS_USER_AUTHORIZED(p_user_name, 'LMRMH') THEN
        v_dynamic_sql := v_dynamic_sql || ' ORDER BY NVL(P.PRIORITY_LEVEL, 0) DESC, B.MUHABERAT_DEADLINE_DATE';
    ELSE
        v_dynamic_sql := v_dynamic_sql || ' ORDER BY NVL(P.PRIORITY_LEVEL, 0) DESC, B.DEADLINE_DATE';
    END IF;

    TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', 'GET_TICKET_LIST', v_dynamic_sql);

    OPEN cur FOR v_dynamic_sql;

    EXCEPTION
    WHEN OTHERS THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
           'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
            'ALZ_LAW_TICKET_PROCESS_UTILS.GET_TICKET_LIST',
            NULL,
            p_process_results);

END;

PROCEDURE GET_USER_STATUS(p_user        IN        VARCHAR2,
                          p_status      OUT       VARCHAR2)

IS

    CURSOR c_user_status IS
    SELECT STATUS
      FROM ALZ_LAW_TICKET_USER_STATUS
     WHERE VALIDITY_START_DATE <= SYSDATE
       AND (VALIDITY_END_DATE IS NULL OR VALIDITY_END_DATE > SYSDATE)
       AND USER_ID = p_user;

BEGIN

     OPEN c_user_status;
    FETCH c_user_status INTO p_status;
    CLOSE c_user_status;

    IF p_status IS NULL THEN
        p_status := 'UYGUN';
    END IF;

END;

PROCEDURE INVOKE_REST(p_payload VARCHAR2, p_url VARCHAR2, p_process_results IN OUT customer.process_result_table)
   IS
   req utl_http.req;
   res utl_http.resp;
   url varchar2(4000) := null;
   buffer varchar2(4000);
   content varchar2(4000) := p_payload;

   begin

       utl_http.set_transfer_timeout(2);

       url := get_url_link(p_url);
       req := utl_http.begin_request(url, 'POST',' HTTP/1.1');
       utl_http.set_header(req, 'user-agent', 'mozilla/4.0');
       utl_http.set_header(req, 'content-type', 'application/json');
       utl_http.set_header(req, 'Content-Length', length(content));

       utl_http.write_text(req, content);
       res := utl_http.get_response(req);

       utl_http.end_response(res);

   exception
      when OTHERS then

         begin
            utl_http.end_response(res);
         exception when OTHERS then
            null;
         end;

         begin
            utl_http.end_request(req);
         exception when OTHERS then
            null;
         end;

         -- TODO BPM çalismasa bile hata alinmasin isteniyor, logluyoruz BPM stabil olunca asagidaki kismi açip log kaldirabiliriz
         TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', 'INVOKE_REST', SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000));
   end;


FUNCTION GET_EMPLOYEE_ID_BY_AZNET_NAME(p_user_name IN VARCHAR2) RETURN VARCHAR2
   IS

   v_employee_id VARCHAR2(30) := NULL;

   CURSOR c_employee_id (username VARCHAR2)
      IS
         SELECT EMPLOYEE_ID FROM WEB_SEC_SYSTEM_USERS WHERE USER_NAME = username;

   BEGIN

   OPEN c_employee_id (p_user_name);
   FETCH c_employee_id INTO v_employee_id;
   CLOSE c_employee_id;

   RETURN v_employee_id;

   END;

PROCEDURE UPDATE_USER_STATUS(p_user              IN     VARCHAR2,
                             p_status            IN     VARCHAR2,
                             p_process_results   IN OUT customer.process_result_table)
IS
    CURSOR c_user_status IS
    SELECT STATUS
      FROM ALZ_LAW_TICKET_USER_STATUS
     WHERE VALIDITY_START_DATE <= SYSDATE
       AND (VALIDITY_END_DATE IS NULL OR VALIDITY_END_DATE >= SYSDATE)
       AND USER_ID = p_user;

    v_status                VARCHAR2 (30) := NULL;

BEGIN

     OPEN c_user_status;
    FETCH c_user_status INTO v_status;
    CLOSE c_user_status;

    IF NVL(v_status, 'X') <> NVL(p_status, 'X') THEN

     UPDATE ALZ_LAW_TICKET_USER_STATUS
        SET VALIDITY_END_DATE = SYSDATE,
            UPDATE_USER = p_user
      WHERE VALIDITY_START_DATE <= SYSDATE
        AND (VALIDITY_END_DATE IS NULL OR VALIDITY_END_DATE >= SYSDATE)
        AND USER_ID = p_user;

     INSERT INTO ALZ_LAW_TICKET_USER_STATUS (USER_ID,
                                             VALIDITY_START_DATE,
                                             STATUS,
                                             UPDATE_USER,
                                             VALIDITY_END_DATE)
                                     VALUES (p_user,
                                             SYSDATE,
                                             p_status,
                                             p_user,
                                             NULL);

    END IF;

    INVOKE_REST('<start><username>'||p_user||'</username></start>', 'http://esb.allianz.com.tr:12000/LitigationRest/soa-infra/resources/default/LitigationProcessManagement/KullaniciDurumRestService/', p_process_results);

EXCEPTION
    WHEN OTHERS
    THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
           'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
            'ALZ_LAW_TICKET_PROCESS_UTILS.UPDATE_USER_STATUS',
            NULL,
            p_process_results);
END;

PROCEDURE UPDATE_ALL_TICKET_STATUS(p_ticket_array       IN      CUSTOMER.NUMBER_ARRAY,
                                   p_status             IN      VARCHAR2,
                                   p_user_name           IN      VARCHAR2,
                                   p_process_results    IN OUT  customer.process_result_table)

IS
    v_array_count       NUMBER := p_ticket_array.count;

BEGIN

    FOR i IN 1 .. v_array_count
    LOOP
        UPDATE ALZ_LAW_TICKET_PROCESS
           SET STATUS = p_status,
           UPDATE_USER = p_user_name,
           UPDATE_DATE = SYSDATE
         WHERE TICKET_ID = p_ticket_array(i);

        ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_TICKET_HISTORY(p_ticket_array(i), 'Durum ' || p_status || ' olarak deðiþtirildi.', p_user_name);
    END LOOP;

EXCEPTION
    WHEN OTHERS THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
           'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
            'ALZ_LAW_TICKET_PROCESS_UTILS.UPDATE_ALL_TICKET_STATUS',
            NULL,
            p_process_results);

END;


PROCEDURE UPDATE_TICKET_STATUS(p_ticket_id        IN      NUMBER,
                               p_status           IN      VARCHAR2,
                               p_user_name        IN      VARCHAR2,
                               p_process_results  IN OUT  customer.process_result_table)
IS

   v_employee_id VARCHAR2(30) := NULL;
   CURSOR c_effected_tickets(v_ticket_id NUMBER)
   IS
        SELECT TICKET_ID
          FROM ALZ_LAW_TICKET
         WHERE TICKET_ID = v_ticket_id
            OR PARENT = v_ticket_id;

BEGIN

    IF p_status in ('CLOSED','READ','CANCELLED') THEN
        v_employee_id := GET_EMPLOYEE_ID_BY_AZNET_NAME(p_user_name);

        FOR ticket_ids in c_effected_tickets(p_ticket_id)
           LOOP
             INVOKE_REST(null,'http://esb.allianz.com.tr:12000/LitigationRest/GenericRestService/resources/myresource/OK/'||v_employee_id||'/'||ticket_ids.ticket_id||'/ ',p_process_results);
           END LOOP;
    END IF;

--    UPDATE ALZ_LAW_TICKET A, ALZ_LAW_TICKET_PROCESS B
--       SET B.STATUS = p_status,
--           B.CLOSE_DATE = DECODE(p_status, 'CLOSED', SYSDATE, B.CLOSE_DATE),
--           B.UPDATE_USER = p_user_name,
--           B.UPDATE_DATE = SYSDATE
--     WHERE A.TICKET_ID = B.TICKET_ID
--       AND (B.TICKET_ID = p_ticket_id
--        OR A.PARENT = p_ticket_id);

    FOR ticket_ids in c_effected_tickets(p_ticket_id)
    LOOP
        UPDATE ALZ_LAW_TICKET_PROCESS
           SET STATUS = p_status,
               CLOSE_DATE = DECODE(p_status, 'CLOSED', SYSDATE, CLOSE_DATE),
               UPDATE_USER = p_user_name,
               UPDATE_DATE = SYSDATE
         WHERE TICKET_ID = ticket_ids.ticket_id;

        ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_TICKET_HISTORY(ticket_ids.ticket_id, 'Durum ' || p_status || ' olarak deðiþtirildi. ', p_user_name);
    END LOOP;

EXCEPTION
    WHEN OTHERS
    THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
           'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
            'ALZ_LAW_TICKET_PROCESS_UTILS.UPDATE_TICKET_STATUS',
            NULL,
            p_process_results);
END;

PROCEDURE UNIFY_TICKET(p_ticket_array       IN      CUSTOMER.NUMBER_ARRAY,
                       p_user_name           IN      VARCHAR2,
                       p_main_ticket_id     OUT     NUMBER,
                       p_process_results    IN OUT  customer.process_result_table)

IS
    v_array_count       NUMBER := p_ticket_array.count;
    v_unified_counter   NUMBER := 0;
    v_deadline_date     DATE;

    v_operation_time    DATE := SYSDATE;
    v_subject           VARCHAR2 (4000) := NULL;
    v_temp_user         VARCHAR2 (30) := '';
    v_department        VARCHAR2 (20) := '';

    two_unified         EXCEPTION;
    nonrucu_defected    EXCEPTION;
    diff_responsible    EXCEPTION;
    not_same_department EXCEPTION;

    CURSOR c_ticket IS
    SELECT B.TICKET_ID, B.DEADLINE_DATE, B.TICKET_TYPE_ID, C.CATEGORY, C.PARENT, B.OWNER, B.DEPARTMENT
      FROM TABLE (CAST (p_ticket_array AS CUSTOMER.NUMBER_ARRAY)) A, ALZ_LAW_TICKET_PROCESS B, ALZ_LAW_TICKET C
     WHERE COLUMN_VALUE = B.TICKET_ID
       AND B.TICKET_ID = C.TICKET_ID
  ORDER BY B.DEADLINE_DATE, B.TICKET_ID;

    r_ticket            c_ticket%ROWTYPE;

BEGIN

    IF v_array_count < 2 THEN

        alz_web_process_utils.process_result (
            0,
            9,
            -1,
            'UNIFY_TICKET',
            'Birleþtirme iþlemi için en az 2 is seçilmelidir.',
            'Birleþtirme iþlemi için en az 2 is seçilmelidir.',
            NULL,
            NULL,
            'ALZ_LAW_TICKET_PROCESS_UTILS.UNIFY_TICKET',
            NULL,
            p_process_results);

            RETURN;
    END IF;

    OPEN c_ticket;
    LOOP
        FETCH c_ticket INTO r_ticket;

        IF NVL(v_department, 'X') <> 'X' THEN

            IF v_department <> r_ticket.DEPARTMENT THEN
                RAISE not_same_department;
            END IF;

        END IF;

        v_department := r_ticket.DEPARTMENT;

        IF NVL(v_temp_user, '') <> '' AND v_temp_user = r_ticket.OWNER THEN
            RAISE diff_responsible;
        END IF;
        v_temp_user := r_ticket.OWNER;

        IF r_ticket.category = 'B' THEN
            v_unified_counter := v_unified_counter + 1;
            IF v_unified_counter = 2 THEN                   -- birden fazla birlestirirlmis is var mi diye baktik. 2 old anda çiktik
                CLOSE c_ticket;
                RAISE two_unified;
            END IF;

            p_main_ticket_id := r_ticket.ticket_id;         -- önceden birlesmis is varsa digerleri de bunun üzerinde birlesecek

        END IF;

        IF r_ticket.ticket_type_id != 4 THEN                -- rücu isi disinda is buldugumuz anda çiktik
            CLOSE c_ticket;
            RAISE nonrucu_defected;
        END IF;

        IF v_deadline_date IS NULL THEN                     -- deadline'i min olan isin id'sini aldik
            v_deadline_date := r_ticket.deadline_date;
        END IF;

        EXIT WHEN c_ticket%NOTFOUND;
    END LOOP;
    CLOSE c_ticket;

    v_subject := 'Ekteki iþler bu iþ altýnda birleþmiþtir: ';
    FOR i IN 1 .. v_array_count
    LOOP
        v_subject := v_subject || ' ' || p_ticket_array(i);
    END LOOP;

    IF NVL(p_main_ticket_id, 0) = 0 THEN

        CREATE_TICKET('BIRLESTIRME', p_user_name, null, v_deadline_date, v_subject, p_user_name, null, v_department, p_main_ticket_id, p_process_results);

        ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_TICKET_HISTORY(p_main_ticket_id, v_subject, p_user_name);

    END IF;

    v_subject := 'Ekteki iþler [' || p_main_ticket_id || '] nolu iþ altýnda birleþmiþtir: ';
    FOR i IN 1 .. v_array_count
    LOOP
        v_subject := v_subject || ' ' || p_ticket_array(i);
    END LOOP;

    FOR i IN 1 .. v_array_count
    LOOP
        IF p_main_ticket_id <> p_ticket_array(i) THEN
            UPDATE ALZ_LAW_TICKET_PROCESS
               SET STATUS = 'UNIFIED',
                   UPDATE_USER = p_user_name,
                   UPDATE_DATE = v_operation_time
             WHERE TICKET_ID = p_ticket_array(i);

            UPDATE ALZ_LAW_TICKET
               SET PARENT = p_main_ticket_id,
                   UPDATE_USER = p_user_name,
                   UPDATE_DATE = v_operation_time,
                   SUBJECT = v_subject
             WHERE TICKET_ID = p_ticket_array(i);

             ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_TICKET_HISTORY(p_ticket_array(i), v_subject, p_user_name);
        END IF;
    END LOOP;

EXCEPTION
    WHEN diff_responsible THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
            'UNIFY_TICKET',
            'Farklý kullanýcýlarýn iþlerini birleþtiremezsiniz.',
            'Farklý kullanýcýlarýn iþlerini birleþtiremezsiniz.',
            NULL,
            NULL,
            'ALZ_LAW_TICKET_PROCESS_UTILS.UNIFY_TICKET',
            NULL,
            p_process_results);

    WHEN two_unified THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
            'UNIFY_TICKET',
            'Birden fazla birleþtirilmiþ iþ üzerinde yeni birleþtirme yapamazsýnýz.',
            'Birden fazla birleþtirilmiþ iþ üzerinde yeni birleþtirme yapamazsýnýz.',
            NULL,
            NULL,
            'ALZ_LAW_TICKET_PROCESS_UTILS.UNIFY_TICKET',
            NULL,
            p_process_results);

    WHEN nonrucu_defected THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
            'UNIFY_TICKET',
            'Birleþtirilecek iþlerin rücu dosyasýna ait iþler olmasý gerekmektedir.',
            'Birleþtirilecek iþlerin rücu dosyasýna ait iþler olmasý gerekmektedir.',
            NULL,
            NULL,
            'ALZ_LAW_TICKET_PROCESS_UTILS.UNIFY_TICKET',
            NULL,
            p_process_results);

    WHEN not_same_department THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
            'UNIFY_TICKET',
            'Birleþtirilecek iþlerin ayný departmana ait iþler olmasý gerekmektedir.',
            'Birleþtirilecek iþlerin ayný departmana ait iþler olmasý gerekmektedir.',
            NULL,
            NULL,
            'ALZ_LAW_TICKET_PROCESS_UTILS.UNIFY_TICKET',
            NULL,
            p_process_results);

    WHEN OTHERS THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
           'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
            'ALZ_LAW_TICKET_PROCESS_UTILS.UNIFY_TICKET',
            NULL,
            p_process_results);

END;

PROCEDURE PAIR_TICKET(p_ticket_array       IN       CUSTOMER.NUMBER_ARRAY,
                      p_user_name          IN       VARCHAR2,
                      p_process_results        OUT  customer.process_result_table)

IS
    v_array_count       NUMBER := p_ticket_array.count;
    v_paired_counter    NUMBER := 0;
    p_main_ticket_id    NUMBER;

    v_main_law_file_no  VARCHAR2 (30) := NULL;
    v_other_law_file_no VARCHAR2 (30) := NULL;

    v_main_uyap_id      VARCHAR2 (30) := NULL;
    v_other_uyap_id     VARCHAR2 (30) := NULL;

    v_main_court_info   VARCHAR2 (30) := NULL;
    v_other_court_info  VARCHAR2 (30) := NULL;

    v_main_owner        VARCHAR2 (30) := NULL;
    v_other_owner       VARCHAR2 (30) := NULL;

    v_is_first_row      BOOLEAN := TRUE;

    v_operation_time    DATE := SYSDATE;
    v_subject           VARCHAR2 (4000) := NULL;

    has_rucu            EXCEPTION;
    two_paired          EXCEPTION;
    cannot_pair         EXCEPTION;

    CURSOR c_ticket IS
    SELECT B.TICKET_ID, B.DEADLINE_DATE, B.TICKET_TYPE_ID, C.CATEGORY, C.PARENT
      FROM TABLE (CAST (p_ticket_array AS CUSTOMER.NUMBER_ARRAY)) A, ALZ_LAW_TICKET_PROCESS B, ALZ_LAW_TICKET C
     WHERE COLUMN_VALUE = B.TICKET_ID
       AND B.TICKET_ID = C.TICKET_ID
  ORDER BY B.DEADLINE_DATE, B.TICKET_ID;

    r_ticket            c_ticket%ROWTYPE;

    CURSOR c_get_law_file_no(p_ticket_id VARCHAR2)
    IS
     SELECT   LAWFILE_NO, UYAP_DOSYA_ID, UYAP_BIRIM_ID || '-' || LAW_COURT_ENF || '-' || COURT_FILE_NO, B.OWNER
       FROM   ALZ_LAW_TICKET A, ALZ_LAW_TICKET_PROCESS B
      WHERE   A.TICKET_ID = B.TICKET_ID
        AND   B.STATUS = 'OPEN'
        AND   A.TICKET_ID = p_ticket_id;

BEGIN

    IF v_array_count < 2 THEN

        alz_web_process_utils.process_result (
            0,
            9,
            -1,
            'UNIFY_TICKET',
            'Eþsleþtirme iþlemi için en az 2 iþ seçilmelidir.',
            'Eþsleþtirme iþlemi için en az 2 iþ seçilmelidir.',
            NULL,
            NULL,
            'ALZ_LAW_TICKET_PROCESS_UTILS.UNIFY_TICKET',
            NULL,
            p_process_results);

            RETURN;
    END IF;

    OPEN c_ticket;
    LOOP
        FETCH c_ticket INTO r_ticket;

        IF v_is_first_row and r_ticket.DEADLINE_DATE IS NOT NULL and p_main_ticket_id IS NULL THEN                             -- deadline'i min olan isin üzerinde eþleþtirecegiz
            v_is_first_row := false;
            p_main_ticket_id := r_ticket.ticket_id;
        END IF;

        IF r_ticket.category = 'E' THEN
            v_paired_counter := v_paired_counter + 1;
            IF v_paired_counter = 2 THEN                   -- birden fazla eþleþtirilmis is var mi diye baktik. 2 old anda çiktik
                CLOSE c_ticket;
                RAISE two_paired;
            END IF;

            p_main_ticket_id := r_ticket.ticket_id;         -- önceden eþleþtirilmis is varsa digerleri de bunun üzerinde eþleþecek

        END IF;

        IF r_ticket.ticket_type_id = 4 THEN                 -- rücu isi buldugumuz anda çiktik
            CLOSE c_ticket;
            RAISE has_rucu;
        END IF;

        EXIT WHEN c_ticket%NOTFOUND;
    END LOOP;
    CLOSE c_ticket;

    v_subject := 'Ekteki iþler eþleþmiþtir: [' || p_main_ticket_id || ']';
    FOR i IN 1 .. v_array_count
    LOOP
        IF p_main_ticket_id <> p_ticket_array(i) THEN
            v_subject := v_subject || ', ' || p_ticket_array(i);
        END IF;
    END LOOP;

    FOR i IN 1 .. v_array_count
    LOOP
        IF p_main_ticket_id <> p_ticket_array(i) THEN

             OPEN c_get_law_file_no (p_main_ticket_id);
            FETCH c_get_law_file_no INTO v_main_law_file_no, v_main_uyap_id, v_main_court_info, v_main_owner;
            CLOSE c_get_law_file_no;

             OPEN c_get_law_file_no (p_ticket_array(i));
            FETCH c_get_law_file_no INTO v_other_law_file_no, v_other_uyap_id, v_other_court_info, v_other_owner;
            CLOSE c_get_law_file_no;

            IF v_main_law_file_no = v_other_law_file_no OR v_main_uyap_id = v_other_uyap_id OR v_main_court_info = v_other_court_info OR v_main_owner = v_other_owner THEN
                UPDATE ALZ_LAW_TICKET_PROCESS
                   SET STATUS = 'PAIRED',
                       UPDATE_USER = p_user_name,
                       UPDATE_DATE = v_operation_time
                 WHERE TICKET_ID = p_ticket_array(i);

                UPDATE ALZ_LAW_TICKET
                   SET PARENT = p_main_ticket_id,
                       SUBJECT = v_subject,
                       UPDATE_USER = p_user_name,
                       UPDATE_DATE = v_operation_time
                 WHERE TICKET_ID = p_ticket_array(i);
            ELSE
                RAISE cannot_pair;
            END IF;

        ELSE
            UPDATE ALZ_LAW_TICKET
               SET CATEGORY = 'E',
                   SUBJECT = v_subject,
                   UPDATE_USER = p_user_name,
                   UPDATE_DATE = v_operation_time
             WHERE TICKET_ID = p_ticket_array(i);
        END IF;

        ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_TICKET_HISTORY(p_ticket_array(i), v_subject, p_user_name);

    END LOOP;

EXCEPTION
    WHEN has_rucu THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
            'PAIR_TICKET',
            'Eþleþtirilecek iþlerin içinde rücu dosyasýna ait iþler olmamasý gerekmektedir.',
            'Eþleþtirilecek iþlerin içinde rücu dosyasýna ait iþler olmamasý gerekmektedir.',
            NULL,
            NULL,
            'ALZ_LAW_TICKET_PROCESS_UTILS.PAIR_TICKET',
            NULL,
            p_process_results);

    WHEN two_paired THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
            'PAIR_TICKET',
            'Birden fazla eþleþtirilmiþ iþ üzerinde yeni eþlestirme yapamazsýnýz.',
            'Birden fazla eþleþtirilmiþ iþ üzerinde yeni eþlestirme yapamazsýnýz.',
            NULL,
            NULL,
            'ALZ_LAW_TICKET_PROCESS_UTILS.PAIR_TICKET',
            NULL,
            p_process_results);

    WHEN cannot_pair THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
            'PAIR_TICKET',
            'Eþlestirme iþlemi için, iþlerin hukuk dosya numaralarýnýn, uyap id lerinin veya esas no larýnýn ve iþ sorumlularýnýn ayný olmasý gerekmektedir.',
            'Eþlestirme iþlemi için, iþlerin hukuk dosya numaralarýnýn, uyap id lerinin veya esas no larýnýn ve iþ sorumlularýnýn ayný olmasý gerekmektedir.',
            NULL,
            NULL,
            'ALZ_LAW_TICKET_PROCESS_UTILS.PAIR_TICKET',
            NULL,
            p_process_results);

    WHEN OTHERS THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
           'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
            'ALZ_LAW_TICKET_PROCESS_UTILS.PAIR_TICKET',
            NULL,
            p_process_results);

END;

PROCEDURE SEPARATE_TICKET(p_ticket_array        IN      CUSTOMER.NUMBER_ARRAY,
                          p_user_name            IN      VARCHAR2,
                          p_process_results     OUT     customer.process_result_table)
IS

    v_array_count       NUMBER := p_ticket_array.count;
    v_parent_ticket     NUMBER;
    v_status            VARCHAR2(10);
    v_extra_child       NUMBER := 0;
    v_type              VARCHAR2(1);
    v_operation_time    DATE := SYSDATE;

BEGIN

    IF v_array_count = 0 THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
            'SEPARATE_TICKET',
            'Ayýrma iþlemi için, ayrýlacak iþ(ler)i seçmeniz gerekmektedir.',
            'Ayýrma iþlemi için, ayrýlacak iþ(ler)i seçmeniz gerekmektedir.',
            NULL,
            NULL,
            'ALZ_LAW_TICKET_PROCESS_UTILS.SEPARATE_TICKET',
            NULL,
            p_process_results);

            RETURN;
    END IF;

    -- ilk ayrilacak ticket'in parent'inin id'sini aldik
    SELECT PARENT
      INTO v_parent_ticket
      FROM ALZ_LAW_TICKET
     WHERE TICKET_ID = p_ticket_array(1);

    -- parent ticket'in type'ini ve statüsünü aldik
    SELECT CATEGORY, STATUS
      INTO v_type, v_status
      FROM ALZ_LAW_TICKET T, ALZ_LAW_TICKET_PROCESS P
     WHERE T.TICKET_ID = v_parent_ticket
       AND T.TICKET_ID = P.TICKET_ID;

    IF v_status = 'CLOSED' THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
            'SEPARATE_TICKET',
            'Kapalý iþ üzerinde ayrýþtýrma iþlemi yapamazsýnýz.',
            'Kapalý iþ üzerinde ayrýþtýrma iþlemi yapamazsýnýz.',
            NULL,
            NULL,
            'ALZ_LAW_TICKET_PROCESS_UTILS.SEPARATE_TICKET',
            NULL,
            p_process_results);

            RETURN;
    END IF;

    -- gelen iþlerin hepsini ayiriyoruz
    FOR i IN 1 .. v_array_count
    LOOP
        UPDATE ALZ_LAW_TICKET_PROCESS
           SET STATUS = 'OPEN',
               UPDATE_USER = p_user_name,
               UPDATE_DATE = v_operation_time
         WHERE TICKET_ID = p_ticket_array(i);

        UPDATE ALZ_LAW_TICKET
           SET PARENT = NULL,
               UPDATE_USER = p_user_name,
               UPDATE_DATE = v_operation_time
         WHERE TICKET_ID = p_ticket_array(i);

        ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_TICKET_HISTORY(p_ticket_array(i), 'Ayristirildi.', p_user_name);
    END LOOP;

    -- ayirma isleminden sonra o parent'a sahip baska ticket var mi diye bakiyoruz
    SELECT COUNT(*)
      INTO v_extra_child
      FROM ALZ_LAW_TICKET
     WHERE PARENT = v_parent_ticket;

    IF (v_extra_child = 0 AND v_type = 'E') OR  (v_extra_child <= 1 AND v_type = 'B') THEN

        UPDATE  ALZ_LAW_TICKET_PROCESS
           SET  STATUS = DECODE(v_type, 'B', 'CANCELLED', 'OPEN'),
                UPDATE_USER = p_user_name,
                UPDATE_DATE = v_operation_time
         WHERE  TICKET_ID = v_parent_ticket;

        UPDATE  ALZ_LAW_TICKET
           SET  CATEGORY = NULL,
                UPDATE_USER = p_user_name,
                UPDATE_DATE = v_operation_time
         WHERE  TICKET_ID = v_parent_ticket;

        IF (v_type = 'B') THEN
            ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_TICKET_HISTORY(v_parent_ticket, 'Ayrýþtýrma dolayýsýyla iptal edildi.', p_user_name);
        ELSIF (v_type = 'E') THEN
            ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_TICKET_HISTORY(v_parent_ticket, 'Ayrýþtýrma dolayýsýyla durum OPEN olarak deðiþtirildi.', p_user_name);
        END IF;

        IF (v_extra_child = 1 AND v_type = 'B') THEN

            SELECT TICKET_ID
              INTO v_extra_child
              FROM ALZ_LAW_TICKET
             WHERE PARENT = v_parent_ticket;

            UPDATE ALZ_LAW_TICKET_PROCESS
               SET STATUS = 'OPEN',
                   UPDATE_USER = p_user_name,
                   UPDATE_DATE = v_operation_time
             WHERE TICKET_ID = (SELECT TICKET_ID FROM ALZ_LAW_TICKET WHERE PARENT = v_parent_ticket);

            UPDATE ALZ_LAW_TICKET
               SET PARENT = NULL,
                   UPDATE_USER = p_user_name,
                   UPDATE_DATE = v_operation_time
             WHERE PARENT = v_parent_ticket;

            ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_TICKET_HISTORY(v_extra_child, 'Ayristirildi.', p_user_name);

        END IF;

    END IF;

EXCEPTION
    WHEN OTHERS THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
           'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
            'ALZ_LAW_TICKET_PROCESS_UTILS.SEPARATE_TICKET',
            NULL,
            p_process_results);
END;

PROCEDURE SET_DEADLINE(p_ticket_id          IN      NUMBER,
                       p_deadline           IN      DATE,
                       p_user_name          IN      VARCHAR2,
                       p_process_results    IN OUT  customer.process_result_table)

IS

BEGIN
    UPDATE ALZ_LAW_TICKET_PROCESS
       SET DEADLINE_DATE = p_deadline,
           UPDATE_USER = p_user_name,
           UPDATE_DATE = SYSDATE
     WHERE ticket_id = p_ticket_id;

    ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_TICKET_HISTORY(p_ticket_id, 'Son bitirme tarihi güncellendi.', p_user_name);

    EXCEPTION
    WHEN OTHERS THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
           'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
            'ALZ_LAW_TICKET_PROCESS_UTILS.SET_DEADLINE',
            NULL,
            p_process_results);
END;

PROCEDURE SET_DOC_RECEIVED(p_ticket_id          IN      NUMBER,
                           p_user_name          IN      VARCHAR2,
                           p_process_results    IN OUT  customer.process_result_table)

IS

BEGIN
    UPDATE ALZ_LAW_TICKET
       SET doc_received = 1,
           UPDATE_USER = p_user_name,
           UPDATE_DATE = SYSDATE
     WHERE ticket_id = p_ticket_id;

    ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_TICKET_HISTORY(p_ticket_id, 'Evrak aslý gelmiþtir olarak iþaretlendi.', p_user_name);

    EXCEPTION
    WHEN OTHERS THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
           'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
            'ALZ_LAW_TICKET_PROCESS_UTILS.SET_DOC_RECEIVED',
            NULL,
            p_process_results);
END;

--TODO YERÝNE REASSIGN_TICKET KULLANILACAK VE BU SÝLÝNEBÝLÝR
--PROCEDURE UPDATE_TICKET_LAWFILE_NO(p_ticket_id          IN      NUMBER,
--                                   p_law_file_no        IN      VARCHAR2,
--                                   p_court_file_no      IN      VARCHAR2,
--                                   p_law_court_no       IN      VARCHAR2,
--                                   p_court_enf_type     IN      VARCHAR2,
--                                   p_user_name          IN      VARCHAR2,
--                                   p_process_results    IN OUT  CUSTOMER.PROCESS_RESULT_TABLE)
--IS

--    CURSOR c_effected_tickets(v_ticket_id NUMBER)
--      IS
--         select *
--           from alz_law_ticket
--          where ticket_id = v_ticket_id
--             or parent = v_ticket_id;

--BEGIN

--    FOR tickets in c_effected_tickets(p_ticket_id)
--        LOOP
--            IF NVL(tickets.uyap_dosya_id, 0) <> 0 THEN
--                alz_litigation_utils.match_ticket_uyap_id(p_law_file_no,
--                                                          tickets.lawfile_no,
--                                                          tickets.uyap_dosya_id,
--                                                          p_user_name,
--                                                          p_process_results);
--            END IF;

--            UPDATE ALZ_LAW_TICKET
--               SET LAWFILE_NO = p_law_file_no,
--                   UPDATE_USER = p_user_name,
--                   UPDATE_DATE = SYSDATE
--             WHERE TICKET_ID = tickets.ticket_id;

--            ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_TICKET_HISTORY(tickets.ticket_id, 'Hukuk dosyasý ile eþleþtirme yapýldý.', p_user_name);
--        END LOOP;

--    EXCEPTION
--    WHEN OTHERS THEN
--        alz_web_process_utils.process_result (
--            0,
--            9,
--            -1,
--           'ORACLE_EXCEPTION',
--            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
--            null,
--            null,
--            null,
--            'ALZ_LAW_TICKET_PROCESS_UTILS.UPDATE_TICKET_LAWFILE_NO',
--            NULL,
--            p_process_results);
--END;

PROCEDURE GET_OPUS_FILE_TYPES(cur OUT refcur)
IS
BEGIN
    OPEN cur FOR
        SELECT LAW_SF_TYPE, EXPLANATION
          FROM KOC_LAW_SF_TYPE
      ORDER BY EXPLANATION;
END;

PROCEDURE GET_UYAP_FILE_TYPES(cur OUT refcur)
IS
BEGIN
    OPEN cur FOR
        SELECT DOSYA_TUR_KODU, DOSYA_TUR_ACIKLAMA
          FROM ALZ_UYAP_DOSYA_TURLERI
      ORDER BY DOSYA_TUR_ACIKLAMA;
END;

PROCEDURE GET_TICKET_TYPE_LIST(cur OUT refcur)
IS
    v_result refcur;
BEGIN
    OPEN v_result FOR
        SELECT DISTINCT TICKET_TYPE_ID, DESCRIPTION
          FROM ALZ_LAW_TICKET_TYPE
      ORDER BY DESCRIPTION;
    cur := v_result;
END;

FUNCTION GET_TIME_PASS (p_create_date IN DATE, p_deadline_DATE IN DATE, p_close_date IN DATE) RETURN NUMBER IS

    V_TIME_PASS         NUMBER := 0;

BEGIN

    IF p_close_date IS NULL THEN
        V_TIME_PASS := ROUND((SYSDATE - p_create_date) * 24 * 60);
    ELSE
        V_TIME_PASS := ROUND((p_close_date - p_create_date) * 24 * 60);
    END IF;

    RETURN V_TIME_PASS;

END GET_TIME_PASS;

FUNCTION GET_TIME_LEFT (P_RECEIVE_DATE IN DATE, p_deadline_DATE IN DATE, p_close_date IN DATE) RETURN NUMBER IS

    V_TIME_LEFT         NUMBER := 0;

BEGIN

    IF p_close_date IS NULL THEN
        V_TIME_LEFT := ROUND((p_deadline_DATE - SYSDATE) * 24 * 60);
    ELSE
        V_TIME_LEFT := 0;
    END IF;

    RETURN V_TIME_LEFT;

END GET_TIME_LEFT;


PROCEDURE UPSERT_TICKET_PRIORITY(p_ticket_id            IN      NUMBER,
                                 p_user_name            IN      VARCHAR,
                                 p_priority_level       IN      NUMBER,
                                 p_process_results      OUT     customer.process_result_table)
IS
    v_update_or_insert  NUMBER;

BEGIN

    SELECT COUNT(ticket_id)
      INTO v_update_or_insert
      FROM ALZ_LAW_TICKET_USER_PRIORITY
     WHERE ticket_id = p_ticket_id
       AND user_name = p_user_name;

    IF NVL(v_update_or_insert, 0) = 0 THEN --INSERT
        INSERT INTO ALZ_LAW_TICKET_USER_PRIORITY (TICKET_ID, USER_NAME, PRIORITY_LEVEL, CREATE_DATE, UPDATE_DATE)
        VALUES (p_ticket_id, p_user_name, p_priority_level, SYSDATE, SYSDATE);

        ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_TICKET_HISTORY(p_ticket_id, 'Ýþ önceliði belirlendi.', p_user_name);

    ELSE -- UPDATE
        UPDATE  ALZ_LAW_TICKET_USER_PRIORITY
           SET  update_date = SYSDATE, priority_level = p_priority_level
         WHERE  ticket_id = p_ticket_id
           AND  user_name = p_user_name;

        ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_TICKET_HISTORY(p_ticket_id, 'Ýþ önceliði güncellendi.', p_user_name);

    END IF;

    EXCEPTION
    WHEN OTHERS THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
           'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
            'ALZ_LAW_TICKET_PROCESS_UTILS.UPSERT_TICKET_PRIORITY',
            NULL,
            p_process_results);

END;


PROCEDURE INSERT_TICKET_ASSIGNMENT_LOG
   IS

   v_new_ticket_id        NUMBER;
   v_process_results      customer.process_result_table;
   v_error                VARCHAR2(10000);

   CURSOR c_result_waiting_files
      IS
         SELECT DOSYA_ID, SIRKET_ADI, UNIQUE_EVRAK_ID, UNIQUE_SAFAHAT_ID
           FROM ALZ_LAW_TICKET_ASSIGNMENT_LOG
          WHERE TICKET_ID IS NULL AND ERROR_EXP IS NULL;

   CURSOR c_result_errors
      IS
         SELECT DOSYA_ID, SIRKET_ADI, UNIQUE_EVRAK_ID, UNIQUE_SAFAHAT_ID
           FROM ALZ_LAW_TICKET_ASSIGNMENT_LOG
          WHERE ERROR_EXP IS NOT NULL;


   BEGIN

         --!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         --!!!!!!!  TODO  ilk hafta gözle takip edilmeli daha sonra sysdat - 10 ' a çevrilmeli
         --!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         INSERT INTO ALZ_LAW_TICKET_ASSIGNMENT_LOG (DOSYA_ID, UNIQUE_EVRAK_ID, UNIQUE_SAFAHAT_ID, SIRKET_ADI)
         SELECT DOSYA_ID, NULL, NULL, SIRKET_ADI
           FROM ALZ_UYAP_DOSYA_LISTESI A
          WHERE KAYIT_TARIHI >= TRUNC(SYSDATE - 1)
            AND VERSION_NO = 0
            AND NOT EXISTS (SELECT 1
                              FROM ALZ_LAW_TICKET_ASSIGNMENT_LOG B
                             WHERE A.DOSYA_ID = B.DOSYA_ID
                               AND A.SIRKET_ADI = B.SIRKET_ADI
                               AND UNIQUE_EVRAK_ID IS NULL
                               AND UNIQUE_SAFAHAT_ID IS NULL);

         INSERT INTO ALZ_LAW_TICKET_ASSIGNMENT_LOG (DOSYA_ID, UNIQUE_EVRAK_ID, UNIQUE_SAFAHAT_ID, SIRKET_ADI)
         SELECT DOSYA_ID, NULL, SAFAHAT_ID, SIRKET_ADI
           FROM ALZ_UYAP_SAFAHAT A
          WHERE KAYIT_TARIHI >= TRUNC(SYSDATE - 1)
            AND NOT EXISTS (SELECT 1
                              FROM ALZ_LAW_TICKET_ASSIGNMENT_LOG B
                             WHERE A.DOSYA_ID = B.DOSYA_ID
                               AND A.SIRKET_ADI = B.SIRKET_ADI
                               AND A.SAFAHAT_ID = B.UNIQUE_SAFAHAT_ID
                               AND UNIQUE_EVRAK_ID IS NULL);

         INSERT INTO ALZ_LAW_TICKET_ASSIGNMENT_LOG (DOSYA_ID, UNIQUE_EVRAK_ID, UNIQUE_SAFAHAT_ID, SIRKET_ADI)
         SELECT DOSYA_ID, UNIQUE_ID, NULL, SIRKET_ADI
           FROM ALZ_UYAP_EVRAKLAR A
          WHERE KAYIT_TARIHI >= TRUNC(SYSDATE - 1)
            AND GUNCEL_VERSIYON = 'Y'
            AND NOT EXISTS (SELECT 1
                              FROM ALZ_UYAP_EVRAKLAR F
                             WHERE F.DOSYA_ID = A.DOSYA_ID
                               AND F.SIRKET_ADI = A.SIRKET_ADI
                               AND F.GG_EVRAK_ID = A.GG_EVRAK_ID
                               AND F.EVRAK_ID = A.EVRAK_ID
                               AND F.VERSION_NO = A.VERSION_NO - 1)
            AND NOT EXISTS (SELECT 1
                              FROM ALZ_LAW_TICKET_ASSIGNMENT_LOG B
                             WHERE A.DOSYA_ID = B.DOSYA_ID
                               AND A.SIRKET_ADI = B.SIRKET_ADI
                               AND A.UNIQUE_ID = B.UNIQUE_EVRAK_ID
                               AND UNIQUE_SAFAHAT_ID IS NULL);

         COMMIT;


         FOR files IN c_result_waiting_files
         LOOP

            v_new_ticket_id := null;
            v_process_results := customer.process_result_table();
            v_error := null;

            IF files.unique_evrak_id IS NULL AND files.unique_safahat_id IS NULL THEN

               CREATE_TICKET('UYAP_DOSYA', null, files.dosya_id , null, files.sirket_adi, 'SYSTEM', null, null, v_new_ticket_id, v_process_results);

            ELSIF files.unique_evrak_id IS NOT NULL THEN

               CREATE_TICKET('UYAP_EVRAK', null, files.unique_evrak_id , null, null, 'SYSTEM', null, null, v_new_ticket_id, v_process_results);

            ELSIF files.unique_safahat_id IS NOT NULL THEN

               CREATE_TICKET('UYAP_SAFAHAT', null, files.unique_safahat_id, null, files.SIRKET_ADI, 'SYSTEM', null, null, v_new_ticket_id, v_process_results);

            END IF;

            IF v_process_results IS NOT NULL AND v_process_results.COUNT > 0 THEN
               v_error := v_process_results(1).reason;
            END IF;

            UPDATE ALZ_LAW_TICKET_ASSIGNMENT_LOG
               SET TICKET_ID = v_new_ticket_id,
                   ERROR_EXP = v_error
             WHERE DOSYA_ID = files.DOSYA_ID
               AND SIRKET_ADI = files.SIRKET_ADI
               AND NVL(UNIQUE_EVRAK_ID,0) = NVL(files.unique_evrak_id,0)
               AND NVL(UNIQUE_SAFAHAT_ID,0) = NVL(files.unique_safahat_id,0);

            COMMIT;

         END LOOP;

         FOR files IN c_result_errors
         LOOP

            v_new_ticket_id := null;
            v_process_results := customer.process_result_table();
            v_error := null;

            IF files.unique_evrak_id IS NULL AND files.unique_safahat_id IS NULL THEN

               CREATE_TICKET('UYAP_DOSYA', null, files.dosya_id , null, files.sirket_adi, 'SYSTEM', null, null, v_new_ticket_id, v_process_results);

            ELSIF files.unique_evrak_id IS NOT NULL THEN

               CREATE_TICKET('UYAP_EVRAK', null, files.unique_evrak_id , null, null, 'SYSTEM', null, null, v_new_ticket_id, v_process_results);

            ELSIF files.unique_safahat_id IS NOT NULL THEN

               CREATE_TICKET('UYAP_SAFAHAT', null, files.unique_safahat_id, null, files.SIRKET_ADI, 'SYSTEM', null, null, v_new_ticket_id, v_process_results);

            END IF;

            IF v_process_results IS NOT NULL AND v_process_results.COUNT > 0 THEN
               v_error := v_process_results(1).reason;
            END IF;

            UPDATE ALZ_LAW_TICKET_ASSIGNMENT_LOG
               SET TICKET_ID = v_new_ticket_id,
                   ERROR_EXP = v_error
             WHERE DOSYA_ID = files.DOSYA_ID
               AND SIRKET_ADI = files.SIRKET_ADI
               AND NVL(UNIQUE_EVRAK_ID,0) = NVL(files.unique_evrak_id,0)
               AND NVL(UNIQUE_SAFAHAT_ID,0) = NVL(files.unique_safahat_id,0);

            COMMIT;

         END LOOP;

   END;

FUNCTION GET_LAWFILE_NO_FROM_COURT_NO(p_court_file_no VARCHAR2) RETURN VARCHAR2
   IS
   v_law_file_no VARCHAR2(50)                     := NULL;
   v_court_file_no VARCHAR2(50)                     := NULL;

   CURSOR c_law_file_from_court_file_no(v_court_file_number VARCHAR2)
      IS
      SELECT B.LAW_FILE_NO
      FROM KOC_LAW_BASES B, KOC_LAW_BASE_COURTS C
      where B.LAW_FILE_NO = C.LAW_FILE_NO
        AND C.COURT_ID IN (SELECT COURT_ID FROM ALZ_LAW_COURTS_V WHERE UPPER(COURT_NAME) LIKE '%TAHKIM%' OR UPPER(COURT_NAME) LIKE '%TAHKÝM%')
        AND UPPER(C.COURT_FILE_NO) LIKE '%E%'
        AND UPPER( REPLACE( REPLACE( REPLACE(C.COURT_FILE_NO,'/','.') , '-' , '.' ) , ' ' , '.' )) =  v_court_file_number
      ORDER BY FILE_OPEN_DATE DESC;

   BEGIN

         OPEN c_law_file_from_court_file_no(p_court_file_no);
         FETCH c_law_file_from_court_file_no INTO v_law_file_no;
         CLOSE c_law_file_from_court_file_no;

      RETURN v_law_file_no;
   END;

PROCEDURE insertTicket (p_new_ticket_id             OUT     NUMBER,
                        p_ticket_type_id            IN      NUMBER,
                        p_owner_group               IN      VARCHAR2,
                        p_owner                     IN      VARCHAR2,
                        p_subject                   IN      VARCHAR2,
                        p_create_user               IN      VARCHAR2,
                        p_department                IN      VARCHAR2,
                        p_category                  IN      VARCHAR2,
                        p_law_file_no               IN      VARCHAR2,
                        p_uyap_file_id              IN      NUMBER,
                        p_filenet_id                IN      VARCHAR2,
                        p_source                    IN      VARCHAR2,
                        p_status                    IN      VARCHAR2,
                        p_law_court_enf             IN      VARCHAR2,
                        p_court_file_no             IN      VARCHAR2,
                        p_uyap_birim_id             IN      VARCHAR2,
                        p_deadline_date             IN      DATE,
                        p_create_date               IN      DATE,
                        p_recieve_date              IN      DATE,
                        p_close_date                IN      DATE,
                        p_rucu_no                   IN      VARCHAR2,
                        p_company_name              IN      VARCHAR2,
                        p_send_mail                 IN      NUMBER,
                        p_uyap_safahat_id           IN      NUMBER,
                        p_uyap_evrak_unique_id      IN      NUMBER,
                        p_header_id                 IN      NUMBER,
                        p_code                      IN      NUMBER,
                        p_arrival_date              IN      DATE,
                        p_process_results           IN OUT     customer.process_result_table)

   IS

         v_operation_time    DATE := SYSDATE;

   BEGIN

         p_new_ticket_id := ALZ_LAW_TICKET_SEQ.nextval;

         INSERT INTO ALZ_LAW_TICKET_PROCESS (TICKET_ID,
                                             TICKET_TYPE_ID,
                                             OWNER_GROUP,
                                             OWNER,
                                             STATUS,
                                             DEADLINE_DATE,
                                             CREATE_USER,
                                             RECEIVE_DATE,
                                             CREATE_DATE,
                                             CLOSE_DATE,
                                             DEPARTMENT,
                                             SEND_MAIL,
                                             UPDATE_USER,
                                             UPDATE_DATE,
                                             ARRIVAL_DATE)
         VALUES                             (p_new_ticket_id,
                                             p_ticket_type_id,
                                             p_owner_group,
                                             p_owner,
                                             p_status,
                                             p_deadline_date,
                                             p_create_user,
                                             p_recieve_date,
                                             p_create_date,
                                             p_close_date,
                                             p_department,
                                             p_send_mail,
                                             p_create_user,
                                             v_operation_time,
                                             p_arrival_date);

         INSERT INTO ALZ_LAW_TICKET(TICKET_ID,
                                    CATEGORY,
                                    LAWFILE_NO,
                                    UYAP_DOSYA_ID,
                                    LAW_COURT_ENF,
                                    UYAP_BIRIM_ID,
                                    COURT_FILE_NO,
                                    SOURCE,
                                    FILENET_ID,
                                    SUBJECT,
                                    RUCU_FILE_NO,
                                    SIRKET_ADI,
                                    UYAP_SAFAHAT_ID,
                                    UYAP_EVRAK_UNIQUE_ID,
                                    HEADER_ID,
                                    CODE,
                                    UPDATE_USER,
                                    UPDATE_DATE)
         VALUES                    (p_new_ticket_id,
                                    p_category,
                                    p_law_file_no,
                                    p_uyap_file_id,
                                    p_law_court_enf,
                                    p_uyap_birim_id,
                                    p_court_file_no,
                                    p_source,
                                    p_filenet_id,
                                    p_subject,
                                    p_rucu_no,
                                    p_company_name,
                                    p_uyap_safahat_id,
                                    p_uyap_evrak_unique_id,
                                    p_header_id,
                                    p_code,
                                    p_create_user,
                                    v_operation_time);

        ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_TICKET_HISTORY(p_new_ticket_id, 'Is olusturuldu.', p_create_user);

   EXCEPTION
   WHEN OTHERS THEN
      alz_web_process_utils.process_result (
            0,
            9,
            -1,
           'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
            'ALZ_LAW_TICKET_PROCESS_UTILS.CREATE_TICKET',
            NULL,
            p_process_results);

   END;

PROCEDURE CREATE_TICKET(p_source_type          IN           VARCHAR2,
                        p_assigned_user        IN           VARCHAR2,
                        p_param_id             IN           VARCHAR2,
                        p_deadline             IN           DATE,
                        p_subject              IN           VARCHAR2,
                        p_create_user          IN           VARCHAR2,
                        p_arrival_date         IN           DATE,
                        p_department           IN           VARCHAR2,
                        p_new_ticket_id             OUT     NUMBER,
                        p_process_results           OUT     customer.process_result_table)
    IS

    v_category                   VARCHAR2(10)    := NULL;
    v_law_court_enf              VARCHAR2(10)    := NULL;
    v_uyap_file_id               NUMBER          := NULL;
    v_law_file_no                VARCHAR2(30)    := NULL;
    v_uyap_birim_id              NUMBER          := NULL;
    v_filenet_id                 VARCHAR2(50)    := NULL;
    v_rucu_no                    VARCHAR2(200)   := NULL;
    v_ticket_type                NUMBER          := NULL;
    v_company_name               VARCHAR2(20)    := NULL;
    v_ticket_status              VARCHAR2(20)    := NULL;
    v_deadline                   DATE;
    v_arrival_date               DATE;
    v_validity_end_date          DATE;
    v_court_file_no              VARCHAR2(50)    := NULL;
    v_uyap_evrak_unique_id       NUMBER          := NULL;
    v_uyap_safahat_id            NUMBER          := NULL;
    v_header_id                  NUMBER          := NULL;
    v_source                     VARCHAR2(50)    := NULL;
    v_create_group               VARCHAR2(50)    := NULL;
    v_send_mail                  NUMBER          := NULL;
    v_subject                    VARCHAR2(4000)  := NULL;
    v_related_subject            VARCHAR2(4000)  := NULL;
    e_group_validity             EXCEPTION;
    e_wrong_source_type          EXCEPTION;

    v_bre_source                 VARCHAR2(50);
    v_bre_is_matching            VARCHAR2(10);

    v_bre_code                   NUMBER;
    v_bre_lehte_aleyhte          VARCHAR2(50);
    v_bre_department             VARCHAR2(20);
    v_bre_in_lawyer_user         VARCHAR2(20);
    v_bre_in_lawyer_group        VARCHAR2(20);
    v_bre_out_lawyer_user        VARCHAR2(20);
    v_bre_out_lawyer_group       VARCHAR2(20);

    v_bre_is_ticket              VARCHAR2(10);
    v_bre_is_notification        VARCHAR2(10);
    v_bre_send_mail              VARCHAR2(10);
    v_bre_term                   NUMBER;
    v_bre_group                  VARCHAR2(20);
    v_bre_is_to_inLawyer         VARCHAR2(10);
    v_bre_is_to_outLawyer        VARCHAR2(10);
    v_bre_user                   VARCHAR2(20);
    v_bre_message                VARCHAR2(200);
    v_safahat_aciklama           VARCHAR2(1000);
    v_bre_in_lawyer_status       VARCHAR2(10);

    v_check_bre_param            NUMBER := 0;
    p_create_ticket_or_notif     NUMBER := 1;

    v_bpm_ticket_type            VARCHAR2(200) := 'Bilgilendirme';

    BEGIN
        -- TODO SÝLNEBÝLÝR...
        TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', '1-CREATE_TICKET-GIRIS', '' || p_source_type || ' - ' ||
                                                                        p_assigned_user || ' - ' ||
                                                                        p_param_id || ' - ' ||
                                                                        p_deadline || ' - ' ||
                                                                        p_subject || ' - ' ||
                                                                        p_create_user || ' - ' ||
                                                                        p_arrival_date || ' - ' ||
                                                                        p_department );
        -- TODO SÝLNEBÝLÝR...
        TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', '2-CREATE_TICKET-CREATE_TICKET_PARAMETERS-GIRIS', p_source_type || ' - ' ||
            p_assigned_user || ' - ' ||
            p_param_id || ' - ' ||
            p_deadline || ' - ' ||
            p_subject || ' - ' ||
            p_create_user || ' - ' ||
            p_arrival_date || ' - ' ||
            p_department || ' - ' ||
            p_new_ticket_id || ' - ' ||
            v_ticket_type || ' - ' ||
            v_bre_group || ' - ' ||
            v_bre_user || ' - ' ||
            v_bre_message || ' - ' ||
            v_subject || ' - ' ||
            v_bre_department || ' - ' ||
            v_category || ' - ' ||
            v_law_file_no || ' - ' ||
            v_uyap_file_id || ' - ' ||
            v_filenet_id || ' - ' ||
            v_source || ' - ' ||
            v_ticket_status || ' - ' ||
            v_law_court_enf || ' - ' ||
            v_court_file_no || ' - ' ||
            v_uyap_birim_id || ' - ' ||
            v_deadline || ' - ' ||
            v_bre_term || ' - ' ||
            v_rucu_no || ' - ' ||
            v_company_name || ' - ' ||
            v_send_mail || ' - ' ||
            v_uyap_safahat_id || ' - ' ||
            v_uyap_evrak_unique_id || ' - ' ||
            v_header_id || ' - ' ||
            v_bre_code || ' - ' ||
            v_arrival_date);

        ALZ_LAW_TICKET_PROCESS_UTILS.CREATE_TICKET_PARAMETERS(
            p_source_type,
            p_assigned_user,
            p_param_id,
            p_deadline,
            p_subject,
            p_create_user,
            p_arrival_date,
            p_department,
            p_new_ticket_id,
            v_ticket_type,
            v_bre_group,
            v_bre_user,
            v_bre_message,
            v_subject,
            v_bre_department,
            v_category,
            v_law_file_no,
            v_uyap_file_id,
            v_filenet_id,
            v_source,
            v_ticket_status,
            v_law_court_enf,
            v_court_file_no,
            v_uyap_birim_id,
            v_deadline,
            v_bre_term,
            v_rucu_no,
            v_company_name,
            v_send_mail,
            v_uyap_safahat_id,
            v_uyap_evrak_unique_id,
            v_header_id,
            v_bre_code,
            v_arrival_date,
            p_process_results,
            0, v_check_bre_param,
            p_create_ticket_or_notif);

            -- TODO SÝLNEBÝLÝR...
            TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', '3-CREATE_TICKET-CREATE_TICKET_PARAMETERS-CIKIS', p_source_type || ' - ' ||
                p_assigned_user || ' - ' ||
                p_param_id || ' - ' ||
                p_deadline || ' - ' ||
                p_subject || ' - ' ||
                p_create_user || ' - ' ||
                p_arrival_date || ' - ' ||
                p_department || ' - ' ||
                p_new_ticket_id || ' - ' ||
                v_ticket_type || ' - ' ||
                v_bre_group || ' - ' ||
                v_bre_user || ' - ' ||
                v_bre_message || ' - ' ||
                v_subject || ' - ' ||
                v_bre_department || ' - ' ||
                v_category || ' - ' ||
                v_law_file_no || ' - ' ||
                v_uyap_file_id || ' - ' ||
                v_filenet_id || ' - ' ||
                v_source || ' - ' ||
                v_ticket_status || ' - ' ||
                v_law_court_enf || ' - ' ||
                v_court_file_no || ' - ' ||
                v_uyap_birim_id || ' - ' ||
                v_deadline || ' - ' ||
                v_bre_term || ' - ' ||
                v_rucu_no || ' - ' ||
                v_company_name || ' - ' ||
                v_send_mail || ' - ' ||
                v_uyap_safahat_id || ' - ' ||
                v_uyap_evrak_unique_id || ' - ' ||
                v_header_id || ' - ' ||
                v_bre_code || ' - ' ||
                v_arrival_date);

        IF p_process_results is not null and p_process_results.count > 0 THEN
            for i in 1..(p_process_results.count) loop
                begin
                    if p_process_results(i).type = -1 then
                        return;
                    end if;
                end;
            end loop;
        END IF;

        IF NVL(p_create_ticket_or_notif, 0) = 0 THEN
            return;
        END IF;

        IF NVL(v_check_bre_param, 0) = 1 AND (v_bre_message IS NOT NULL OR v_bre_user IS NULL) THEN

          alz_web_process_utils.process_result (
              0,
              9,
              -1,
              'CREATE_TICKET',
              NVL(v_bre_message, 'BRE den kullanýcý alýnamadý'),
              NVL(v_bre_message, 'BRE den kullanýcý alýnamadý'),
              NULL,
              NULL,
              'ALZ_LAW_TICKET_PROCESS_UTILS.CREATE_TICKET',
              NULL,
              p_process_results);

          RETURN;

        elsif v_bre_user IS NULL and v_bre_group is null then
            alz_web_process_utils.process_result (
              0,
              9,
              -1,
              'CREATE_TICKET',
              NVL(v_bre_message, 'BRE den kullanýcý alýnamadý'),
              NVL(v_bre_message, 'BRE den kullanýcý alýnamadý'),
              NULL,
              NULL,
              'ALZ_LAW_TICKET_PROCESS_UTILS.CREATE_TICKET',
              NULL,
              p_process_results);

          RETURN;

        ELSE
            insertTicket(p_new_ticket_id,
                        v_ticket_type,
                        v_bre_group,
                        v_bre_user,
                        v_subject,
                        p_create_user,
                        v_bre_department,
                        v_category,
                        v_law_file_no,
                        v_uyap_file_id,
                        v_filenet_id,
                        v_source,
                        v_ticket_status,
                        v_law_court_enf,
                        v_court_file_no,
                        v_uyap_birim_id,
                        v_deadline,
                        sysdate,          --p_create_date
                        sysdate,          --p_recieve_date
                        null,             --p_close_date
                        v_rucu_no,
                        v_company_name,
                        v_send_mail,
                        v_uyap_safahat_id,
                        v_uyap_evrak_unique_id,
                        v_header_id,
                        v_bre_code,
                        v_arrival_date,
                        p_process_results);
        END IF;


        IF p_source_type IN ('RUCU', 'BIRLESTIRME') THEN

            INVOKE_REST('<start><ticketId>'||p_new_ticket_id||'</ticketId><username>'||v_bre_user||'</username></start>', 'http://esb.allianz.com.tr:12000/LitigationRest/soa-infra/resources/default/LitigationProcessManagement/RucuSevkRestService/',p_process_results);

        ELSIF p_source_type = 'MUHABERAT' THEN

            IF v_ticket_type <> 1 THEN
                v_bpm_ticket_type := 'Muhaberat';
            END IF;

            INVOKE_REST('<start><source>'||v_bpm_ticket_type||'</source><ticketId>'||p_new_ticket_id||'</ticketId><username>'||v_bre_user||'</username></start>', 'http://esb.allianz.com.tr:12000/LitigationRest/soa-infra/resources/default/LitigationProcessManagement/GenelHukukRestService/',p_process_results);

        ELSIF p_source_type = 'UYAP_EVRAK' THEN

            IF v_ticket_type <> 1 THEN
                v_bpm_ticket_type := 'Evrak';
            END IF;

            INVOKE_REST('<start><ticketId>'||p_new_ticket_id||'</ticketId><source>'||v_bpm_ticket_type||'</source><username>'||v_bre_user||'</username></start>', 'http://esb.allianz.com.tr:12000/LitigationRest/soa-infra/resources/default/LitigationProcessManagement/UyapSevkRestService/',p_process_results);

        ELSIF p_source_type = 'UYAP_SAFAHAT' THEN

            IF v_ticket_type <> 1 THEN
                v_bpm_ticket_type := 'Safahat';
            END IF;

            INVOKE_REST('<start><ticketId>'||p_new_ticket_id||'</ticketId><source>'||v_bpm_ticket_type||'</source><username>'||v_bre_user||'</username></start>', 'http://esb.allianz.com.tr:12000/LitigationRest/soa-infra/resources/default/LitigationProcessManagement/UyapSevkRestService/',p_process_results);

        ELSIF p_source_type = 'UYAP_DOSYA' THEN

            INVOKE_REST('<start><ticketId>'||p_new_ticket_id||'</ticketId><source>Yeni Dosya Bilgilendirme</source><username>'||v_bre_user||'</username></start>', 'http://esb.allianz.com.tr:12000/LitigationRest/soa-infra/resources/default/LitigationProcessManagement/UyapSevkRestService/',p_process_results);

        ELSIF p_source_type = 'TAHKIM' THEN
            IF v_bre_code in (35, 38, 41, 44) THEN
                v_bpm_ticket_type := 'Tahkim Bilgilendirme';
            ELSIF v_bre_code = 871 THEN
                v_bpm_ticket_type := 'Çözümlenemeyen Tahkim Mail';
            ELSE
                v_bpm_ticket_type := 'Tahkim Is';
            END IF;

            INVOKE_REST('<start><ticketId>'||p_new_ticket_id||'</ticketId><source>'||v_bpm_ticket_type||'</source><username>'||v_bre_user||'</username></start>', 'http://esb.allianz.com.tr:12000/LitigationRest/soa-infra/resources/default/LitigationProcessManagement/TahkimMailRestService/',p_process_results);

        ELSIF p_source_type = 'MANUEL' THEN

            v_bpm_ticket_type := 'Manuel';

            INVOKE_REST('<start><source>'||v_bpm_ticket_type||'</source><ticketId>'||p_new_ticket_id||'</ticketId><username>'||v_bre_user||'</username></start>', 'http://esb.allianz.com.tr:12000/LitigationRest/soa-infra/resources/default/LitigationProcessManagement/GenelHukukRestService/',p_process_results);

        END IF;

    EXCEPTION
    WHEN e_wrong_source_type THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
            'CREATE_TICKET',
            'Geçersiz kaynak tipi.',
            'Geçersiz kaynak tipi.',
            NULL,
            NULL,
            'ALZ_LAW_TICKET_PROCESS_UTILS.CREATE_TICKET',
            NULL,
            p_process_results);

    WHEN e_group_validity THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
            'CREATE_TICKET',
            'Manuel atanan kiþi bir gruba kayýtlý deðil veya geçerlilik süresi dolu.',
            'Manuel atanan kiþi bir gruba kayýtlý deðil veya geçerlilik süresi dolu.',
            NULL,
            NULL,
            'ALZ_LAW_TICKET_PROCESS_UTILS.CREATE_TICKET',
            NULL,
            p_process_results);
    WHEN OTHERS THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
            'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
            'ALZ_LAW_TICKET_PROCESS_UTILS.CREATE_TICKET',
            NULL,
            p_process_results);

    END;


PROCEDURE CREATE_TICKET_PARAMETERS(p_source_type            IN           VARCHAR2,
                                   p_assigned_user          IN           VARCHAR2,
                                   p_param_id               IN           VARCHAR2,
                                   p_deadline               IN           DATE,
                                   p_subject                IN           VARCHAR2,
                                   p_create_user            IN           VARCHAR2,
                                   p_arrival_date           IN           DATE,
                                   p_department             IN           VARCHAR2,
                                   p_new_ticket_id          IN   OUT     NUMBER,
                                   v_ticket_type                 OUT     NUMBER,
                                   v_bre_group                   OUT     VARCHAR2,
                                   v_bre_user                    OUT     VARCHAR2,
                                   v_bre_message                 OUT     VARCHAR2,
                                   v_subject                     OUT     VARCHAR2,
                                   v_bre_department              OUT     VARCHAR2,
                                   v_category                    OUT     VARCHAR2,
                                   v_law_file_no            IN   OUT     VARCHAR2,
                                   v_uyap_file_id                OUT     NUMBER,
                                   v_filenet_id                  OUT     VARCHAR2,
                                   v_source                      OUT     VARCHAR2,
                                   v_ticket_status               OUT     VARCHAR2,
                                   v_law_court_enf               OUT     VARCHAR2,
                                   v_court_file_no               OUT     VARCHAR2,
                                   v_uyap_birim_id               OUT     NUMBER,
                                   v_deadline                    OUT     DATE,
                                   v_bre_term                    OUT     NUMBER,
                                   v_rucu_no                     OUT     VARCHAR2,
                                   v_company_name                OUT     VARCHAR2,
                                   v_send_mail                   OUT     NUMBER,
                                   v_uyap_safahat_id             OUT     NUMBER,
                                   v_uyap_evrak_unique_id        OUT     NUMBER,
                                   v_header_id                   OUT     NUMBER,
                                   v_bre_code                    OUT     NUMBER,
                                   v_arrival_date                OUT     DATE,
                                   p_process_results             OUT     customer.process_result_table,
                                   p_reAssign               IN           NUMBER default 0,
                                   p_check_bre_param        IN   OUT     NUMBER,
                                   p_create_ticket_or_notif IN   OUT     NUMBER)
   IS

   v_validity_end_date          DATE;
   v_create_group               VARCHAR2(50)    := NULL;
   v_related_subject            VARCHAR2(4000)  := NULL;
   e_group_validity             EXCEPTION;
   e_wrong_source_type          EXCEPTION;

   v_bre_source                 VARCHAR2(50);
   v_bre_is_matching            VARCHAR2(10);

   v_bre_lehte_aleyhte          VARCHAR2(50);
   v_bre_in_lawyer_user         VARCHAR2(20);
   v_bre_in_lawyer_group        VARCHAR2(20);
   v_bre_out_lawyer_user        VARCHAR2(20);
   v_bre_out_lawyer_group       VARCHAR2(20);

   v_bre_is_ticket              VARCHAR2(10);
   v_bre_is_notification        VARCHAR2(10);
   v_bre_send_mail              VARCHAR2(10);
   v_bre_is_to_inLawyer         VARCHAR2(10);
   v_bre_is_to_outLawyer        VARCHAR2(10);
   v_safahat_aciklama           VARCHAR2(1000);
   v_bre_in_lawyer_status       VARCHAR2(10);

   v_inLawyer_group             VARCHAR2(20);

   temp_v_law_file_no VARCHAR2(50);

   isOutLawyer                  NUMBER;
   isInLawyer                   NUMBER;

   v_bpm_ticket_type            VARCHAR2(200) := 'Bilgilendirme';


   CURSOR c_related_tickets(v_law_file_no VARCHAR2, v_uyap_file_id NUMBER, v_doc_code NUMBER)
      IS
         SELECT B.TICKET_ID, B.OWNER, B.OWNER_GROUP FROM ALZ_LAW_TICKET A, ALZ_LAW_TICKET_PROCESS B
          WHERE A.TICKET_ID = B.TICKET_ID
            AND A.CODE = v_doc_code
            AND (A.LAWFILE_NO = NVL(v_law_file_no, '-1')
                 OR A.UYAP_DOSYA_ID = NVL(v_uyap_file_id, -1))
            AND A.SOURCE IN ('UYAP', 'MUHABERAT')
            AND B.STATUS IN ('OPEN', 'UNIFIED');

   CURSOR c_taraf_rolu(v_uyap_file_id NUMBER, v_company_name VARCHAR2)
      IS
         SELECT DECODE(TARAF_ROL_KODU , 1, 'FAVORABLE', 'UNFAVORABLE') FAVOR
         FROM ALZ_UYAP_TARAFLAR t
         WHERE t.GUNCEL_VERSIYON = 'Y'
         AND t.DOSYA_ID = v_uyap_file_id
         AND VERGI_NO = '8000013270'
         AND SIRKET_ADI = v_company_name
         AND TARAF_ROL_KODU in (1,2);

   CURSOR c_owner_group(v_user_name VARCHAR2)
      IS
         SELECT group_code, validity_end_date
         FROM   ALZ_LAW_GROUP_USER_REL
         WHERE  username = v_user_name
         AND validity_start_date <= SYSDATE
         AND (validity_end_date >= SYSDATE or validity_end_date is null);

   CURSOR c_court_info(v_lawfileno VARCHAR2)
      IS
         select court_type, court_id, court_file_no
           from
              (
               select row_number() over(partition by lbc.law_file_no order by decode(lbc.court_enf_type, 'M', 1, 'R', 2, 'Y', 3, 'I', 4, 'T', 5, 'S', 6, 'H', 7, 'A', 8)) as row_num, lbc.*
                 from koc_law_base_courts lbc
              )
          where row_num = 1
            and law_file_no = v_lawfileno;


   CURSOR c_uyap_file_info(v_uyap_file_id NUMBER, v_company_name VARCHAR2)
      IS
         SELECT L.BIRIM_ID,
                L.DOSYA_NO
           FROM ALZ_UYAP_DOSYA_LISTESI L
          WHERE DOSYA_ID = v_uyap_file_id
            AND SIRKET_ADI = v_company_name
            AND GUNCEL_VERSIYON = 'Y';

   v_uyap_file_info_rec     c_uyap_file_info%rowtype;

   CURSOR c_result_evrak (v_unique_id NUMBER)
      IS
       SELECT E.DOSYA_ID,
              E.SIRKET_ADI,
              I.FILENET_ID,
              C.CODE,
              L.BIRIM_ID,
              L.DOSYA_NO
         FROM ALZ_UYAP_EVRAKLAR        E,
              ALZ_LAW_DOC_TYPE_DEF     C,
              ALZ_UYAP_EVRAK_ICERIK    I,
              ALZ_UYAP_DOSYA_LISTESI   L
        WHERE E.UNIQUE_ID = v_unique_id
          AND REPLACE(LOWER(E.TURU), CHR(10),' ') = LOWER(C.DOC_DESC(+))
          AND I.DOSYA_ID(+) = E.DOSYA_ID
          AND I.EVRAK_ID(+) = E.EVRAK_ID
          AND I.EK_EVRAK(+) = 'N'
          AND I.SIRKET_ADI(+) = E.SIRKET_ADI
          AND L.SIRKET_ADI(+) = E.SIRKET_ADI
          AND L.DOSYA_ID(+) = E.DOSYA_ID
          AND L.GUNCEL_VERSIYON(+) = 'Y';


   CURSOR c_result_safahat(v_unique_safahat_id NUMBER, v_company_name VARCHAR2)
      IS
       SELECT S.DOSYA_ID,
              S.SEFAHAT_TURU_KOD,
              L.DOSYA_NO,
              L.BIRIM_ID,
              S.ACIKLAMA
         FROM ALZ_UYAP_SAFAHAT S,
              ALZ_UYAP_DOSYA_LISTESI L
        WHERE S.SAFAHAT_ID = v_unique_safahat_id
          AND S.SIRKET_ADI = v_company_name
          AND L.SIRKET_ADI = S.SIRKET_ADI
          AND L.DOSYA_ID = S.DOSYA_ID
          AND L.GUNCEL_VERSIYON = 'Y';

   CURSOR c_law_file_info_by_filenet_id (v_filenetid VARCHAR2)
      IS
         SELECT  T.DOCUMENT_CODE,
                 T.DEPARTMENT_CODE,
                 C.COMPANY,
                 C.COURT_FILE_NO,
                 C.LAW_COURT_NO,
                 C.LAW_COURT_TYPE,
                 C.LAW_FILE_NO,
                 C.UYAP_ID
           FROM  ALZ_CLM_COM_DOCS_INDEX_TBL T, ALZ_LAW_COM_TBL C
          WHERE  T.FILENET_ID = v_filenetid
            AND  T.COMMUNICATION_NO = C.COMMUNICATION_NO;

   v_law_file_info_by_filenet_rec     c_law_file_info_by_filenet_id%rowtype;

   CURSOR c_owner_info_by_filenet_id (v_filenetid VARCHAR2)
      IS
         SELECT  B.OWNER
           FROM  ALZ_LAW_TICKET A, ALZ_LAW_TICKET_PROCESS B
          WHERE  A.TICKET_ID = B.TICKET_ID
            AND  A.FILENET_ID = v_filenetid;

   CURSOR c_ticket_info_by_ticket_id (v_ticket_id NUMBER)
      IS
         SELECT  B.DEPARTMENT
           FROM  ALZ_LAW_TICKET A, ALZ_LAW_TICKET_PROCESS B
          WHERE  A.TICKET_ID = B.TICKET_ID
            AND  A.TICKET_ID = v_ticket_id;

   CURSOR c_law_file_info_by_lawfile_no(v_lawfileno VARCHAR2)
      IS
         SELECT L.AZNET_USER IN_LAWYER_USER,
                DECODE(L.AZNET_USER, NULL, NULL, IN_LAWYER_GROUP) IN_LAWYER_GROUP,
                W.USER_NAME OUT_LAWYER_USER,
                DECODE(W.USER_NAME, NULL, NULL, 'LMHA') OUT_LAWYER_GROUP,
                DECODE(B.LAW_CASE_TYPE , 'L', 'FAVORABLE', 'UNFAVORABLE') FAVOR,
                DECODE(B.SUPPLIER_TYPE , 'HM', 'KDT', 'HDT') DEPARTMENT
           FROM KOC_LAW_BASES B, ALZ_LAWYERS L, WEB_SEC_SYSTEM_USERS W
          WHERE LAW_FILE_NO = v_lawfileno
                AND IN_LAWYER = L.ID(+)
                AND LAWYER = CUSTOMER_PARTNER_ID(+);

   v_law_file_info_by_lawfile_rec     c_law_file_info_by_lawfile_no%rowtype;

   CURSOR c_rucu_law_file_no(v_rucu_dosya_no VARCHAR2)
      IS
         SELECT B.law_file_no
         FROM KOC_LAW_BASES_DETAIL B, KOC_CLM_RECOURSE_DETAIL C
         WHERE C.CLAIM_ID = B.CLAIM_ID
         AND C.SF_NO =B.SF_NO
         AND EXT_REFERENCE = v_rucu_dosya_no;


    CURSOR c_law_court_tahkim
      IS
         SELECT COURT_TYPE,
                COURT_ID/*,
                COURT_NAME*/
           FROM ALZ_LOOK_UP A,
                ALZ_LAW_COURTS_V B
          WHERE A.CODE = 'LAW_TAHKIM_DEFAULT'
            AND COURT_TYPE||COURT_ID = A.PARAMETER;


   CURSOR c_lawfile_tahkim (v_header_id NUMBER)
      IS
         SELECT H.LAW_FILE_NO,
                H.COURT_FILE_NO,
                D.DOCUMENT_CODE
           FROM KOC_LAW_EXT_DOCUMENTS_HDR H, KOC_LAW_EXT_DOCUMENTS_DTL D
          WHERE H.HEADER_ID = D.HEADER_ID (+)
            AND H.HEADER_ID = v_header_id
            AND D.DOCUMENT_CODE <> 1011;

   CURSOR c_lawfile_tahkim_from_ticket (v_header_id NUMBER)
      IS
         SELECT LAWFILE_NO,
                COURT_FILE_NO,
                CODE DOCUMENT_CODE
           FROM ALZ_LAW_TICKET
          WHERE HEADER_ID = v_header_id;

   BEGIN

      v_ticket_status := 'OPEN';

      v_bre_source := p_source_type;
      v_source := p_source_type;
      v_subject := p_subject;
      v_company_name := 'AZS';

      v_bre_department := p_department;

      v_bre_is_matching := 'NO';

      v_send_mail := 0;

      v_arrival_date := NVL(p_arrival_date,SYSDATE);

      IF p_source_type IN ('MANUEL', 'BIRLESTIRME') THEN

         v_ticket_type := 5;

         IF p_source_type = 'BIRLESTIRME' THEN
            v_category := 'B';
            v_ticket_type := 4;
            v_source := 'RUCU';
         END IF;

         IF NVL(P_REASSIGN, 0) = 0 AND NVL(v_law_file_no, 'X') = 'X' THEN
            v_law_file_no := p_param_id;
         END IF;

         v_bre_user := p_assigned_user;

         OPEN  c_owner_group (p_assigned_user);
         FETCH c_owner_group INTO v_bre_group, v_validity_end_date;
         CLOSE c_owner_group;

         IF v_bre_group IS NULL OR v_validity_end_date < SYSDATE THEN
            RAISE e_group_validity;
         END IF;

      ELSIF p_source_type = 'DOSYA' THEN

         v_bre_user  := p_assigned_user;

         IF NVL(P_REASSIGN, 0) = 0 AND NVL(v_law_file_no, 'X') = 'X' THEN
            v_law_file_no := p_param_id;
         END IF;

         OPEN  c_court_info (v_law_file_no);
         FETCH c_court_info INTO v_law_court_enf, v_uyap_birim_id, v_court_file_no;
         CLOSE c_court_info;

         select count(1)
           into isOutLawyer
           from alz_law_group_user_rel
          where username = p_assigned_user
            and group_code = 'LMHA';

         IF NVL(isOutLawyer, 0) = 1 THEN
             v_bre_group := 'LMHA';
         ELSE
             select   count (1)
               into   isInLawyer
               from   alz_law_group_user_rel
              where   username = p_assigned_user;

             IF NVL(isInLawyer, 0) = 1 THEN
                begin
                    select   group_code
                      into   v_inLawyer_group
                      from   alz_law_group_user_rel
                     where   username = p_assigned_user;

                    v_bre_group := v_inLawyer_group;
                 exception when others then
                     v_inLawyer_group := NULL;
                 end;

             ELSIF NVL(isInLawyer, 0) > 1 THEN
                begin
                    select   in_lawyer_group
                      into   v_inLawyer_group
                      from   koc_law_bases
                     where   law_file_no = p_param_id;

                    v_bre_group := v_inLawyer_group;
                 exception when others then
                     v_inLawyer_group := NULL;
                 end;
             END IF;

         END IF;

         v_ticket_type := 9;
         v_source := 'DOSYA';

      ELSIF p_source_type = 'OTORIZASYON' THEN

         IF NVL(P_REASSIGN, 0) = 0 AND NVL(v_law_file_no, 'X') = 'X' THEN
            v_law_file_no := p_param_id;
         END IF;

         v_deadline := p_deadline;

         OPEN  c_court_info (v_law_file_no);
         FETCH c_court_info INTO v_law_court_enf, v_uyap_birim_id, v_court_file_no;
         CLOSE c_court_info;

         begin
             select   in_lawyer_group
               into   v_inLawyer_group
               from   koc_law_bases
              where   law_file_no = p_param_id;
         exception when others then
             v_inLawyer_group := NULL;
         end;

         v_bre_group := v_inLawyer_group;
         v_bre_user := p_assigned_user;

         v_bre_in_lawyer_user := p_assigned_user;
         v_bre_in_lawyer_group := v_inLawyer_group;

         v_ticket_type := 10;
         v_source := 'OTORIZASYON';

      ELSIF p_source_type = 'RUCU' THEN

         v_ticket_type := 4;
         v_bre_department := p_department;
         v_rucu_no := p_param_id;

      ELSIF p_source_type = 'TAHKIM' THEN

         v_ticket_type := 8;

         v_bre_source := 'EVRAK';

         v_header_id := TO_NUMBER(p_param_id);

         IF NVL(P_REASSIGN, 0) = 0 THEN
            OPEN c_lawfile_tahkim(v_header_id);
           FETCH c_lawfile_tahkim INTO temp_v_law_file_no, v_court_file_no, v_bre_code;
           CLOSE c_lawfile_tahkim;
         ELSIF NVL(P_REASSIGN, 0) = 1 THEN
            OPEN c_lawfile_tahkim_from_ticket(v_header_id);
           FETCH c_lawfile_tahkim_from_ticket INTO temp_v_law_file_no, v_court_file_no, v_bre_code;
           CLOSE c_lawfile_tahkim_from_ticket;
         END IF;

         IF NVL(v_law_file_no, 'X') = 'X' THEN
            v_law_file_no := temp_v_law_file_no;
         END IF;

          OPEN c_law_court_tahkim;
         FETCH c_law_court_tahkim INTO v_law_court_enf,  v_uyap_birim_id;
         CLOSE c_law_court_tahkim;

         IF NVL(v_law_file_no, 'X') = 'X' THEN
            IF NVL(P_REASSIGN, 0) = 0 THEN
                v_law_file_no := GET_LAWFILE_NO_FROM_COURT_NO(v_court_file_no);
            END IF;
         END IF;

         IF NVL(v_law_file_no, 'X') = 'X' THEN
            v_bre_lehte_aleyhte := 'UNFAVORABLE';     -- DEFAULT ALEYYH
         END IF;

      ELSIF p_source_type = 'MUHABERAT' THEN

         v_ticket_type := 2;

         v_bre_source := 'EVRAK';

         v_filenet_id := p_param_id;

         OPEN c_law_file_info_by_filenet_id(v_filenet_id);
         FETCH c_law_file_info_by_filenet_id INTO v_law_file_info_by_filenet_rec;
         CLOSE c_law_file_info_by_filenet_id;

         v_company_name := v_law_file_info_by_filenet_rec.company;
         v_bre_code := TO_NUMBER(v_law_file_info_by_filenet_rec.document_code);
         v_uyap_birim_id := v_law_file_info_by_filenet_rec.law_court_no;
         v_law_court_enf := v_law_file_info_by_filenet_rec.law_court_type;
         v_court_file_no := v_law_file_info_by_filenet_rec.court_file_no;

         v_uyap_file_id := v_law_file_info_by_filenet_rec.uyap_id;

         if nvl(v_law_file_no, 'X') = 'X' then
            IF NVL(P_REASSIGN, 0) = 0 THEN
                OPEN c_law_file_info_by_filenet_id(v_filenet_id);
                FETCH c_law_file_info_by_filenet_id INTO v_law_file_info_by_filenet_rec;
                CLOSE c_law_file_info_by_filenet_id;

                v_law_file_no := v_law_file_info_by_filenet_rec.law_file_no;
            END IF;
         end if;

         if P_REASSIGN = 1 then
            OPEN c_owner_info_by_filenet_id(v_filenet_id);
           FETCH c_owner_info_by_filenet_id INTO v_bre_in_lawyer_user;
           CLOSE c_owner_info_by_filenet_id;
         end if;

         IF nvl(v_uyap_file_id, 0) > 0 THEN

            OPEN c_taraf_rolu (v_uyap_file_id, v_company_name);
            FETCH c_taraf_rolu INTO v_bre_lehte_aleyhte;
            CLOSE c_taraf_rolu;

            v_bre_lehte_aleyhte := NVL(v_bre_lehte_aleyhte, 'UNFAVORABLE');     -- UYAPDA AYIRT EDEMEZSE DEFAULT ALEYYH

         ELSE       --DOSYASIZ GIRIS

            v_bre_lehte_aleyhte := 'UNFAVORABLE';

         END IF;

         IF NVL(P_REASSIGN, 0) = 1 THEN
            IF NVL(v_law_file_no, 'X') = 'X' OR
               koc_law_utils.check_law_file(v_law_file_no) = 0
            THEN
                OPEN c_ticket_info_by_ticket_id(p_new_ticket_id);
               FETCH c_ticket_info_by_ticket_id INTO v_bre_department;
               CLOSE c_ticket_info_by_ticket_id;

               v_bre_lehte_aleyhte := 'UNFAVORABLE';
            ELSE
                OPEN c_law_file_info_by_lawfile_no(v_law_file_no);
               FETCH c_law_file_info_by_lawfile_no INTO v_law_file_info_by_lawfile_rec;
               CLOSE c_law_file_info_by_lawfile_no;

               v_bre_department := v_law_file_info_by_lawfile_rec.department;
               v_bre_lehte_aleyhte := v_law_file_info_by_lawfile_rec.favor;
            END IF;
         END IF;

         IF NVL(v_bre_department, 'X') = 'X' AND NVL(v_law_file_no, 'X') = 'X' THEN
            v_bre_department := GET_DEPARTMENT(v_company_name, v_law_court_enf, v_uyap_birim_id, v_bre_code);
         END IF;

      ELSIF p_source_type = 'UYAP_EVRAK' THEN

         v_ticket_type := 2;

         v_bre_source := 'EVRAK';
         v_source := 'UYAP';

         v_uyap_evrak_unique_id := TO_NUMBER(p_param_id);


         OPEN  c_result_evrak (v_uyap_evrak_unique_id);
         FETCH c_result_evrak INTO v_uyap_file_id, v_company_name, v_filenet_id, v_bre_code, v_uyap_birim_id, v_court_file_no;
         CLOSE c_result_evrak;

         v_law_court_enf := 'U';

         IF NVL(P_REASSIGN, 0) = 0 AND NVL(v_law_file_no, 'X') = 'X' THEN
            v_law_file_no := alz_litigation_utils.match_uyap_law_file(v_uyap_file_id, v_uyap_birim_id, v_court_file_no);
         END IF;

         IF NVL(v_law_file_no, 'X') = 'X' THEN

             OPEN c_taraf_rolu (v_uyap_file_id, v_company_name);
             FETCH c_taraf_rolu INTO v_bre_lehte_aleyhte;
             CLOSE c_taraf_rolu;

             v_bre_lehte_aleyhte := NVL(v_bre_lehte_aleyhte, 'UNFAVORABLE');     -- UYAPDA AYIRT EDEMEZSE DEFAULT ALEYYH

             v_bre_department := GET_DEPARTMENT(v_company_name, v_law_court_enf, v_uyap_birim_id, v_bre_code);

         END IF;

         IF NVL(P_REASSIGN, 0) = 1 THEN
            IF NVL(v_law_file_no, 'X') = 'X' OR
               koc_law_utils.check_law_file(v_law_file_no) = 0
            THEN
                OPEN c_ticket_info_by_ticket_id(p_new_ticket_id);
               FETCH c_ticket_info_by_ticket_id INTO v_bre_department;
               CLOSE c_ticket_info_by_ticket_id;

               v_bre_lehte_aleyhte := 'UNFAVORABLE';
            ELSE
                OPEN c_law_file_info_by_lawfile_no(v_law_file_no);
               FETCH c_law_file_info_by_lawfile_no INTO v_law_file_info_by_lawfile_rec;
               CLOSE c_law_file_info_by_lawfile_no;

               v_bre_department := v_law_file_info_by_lawfile_rec.department;
               v_bre_lehte_aleyhte := v_law_file_info_by_lawfile_rec.favor;
            END IF;
         END IF;

      ELSIF p_source_type = 'UYAP_SAFAHAT' THEN

         v_ticket_type := 7;

         v_bre_source := 'SAFAHAT';
         v_source := 'UYAP';

         v_uyap_safahat_id := TO_NUMBER(p_param_id);
         v_company_name := p_subject;

         OPEN  c_result_safahat(v_uyap_safahat_id, v_company_name);
         FETCH c_result_safahat INTO v_uyap_file_id, v_bre_code, v_court_file_no, v_uyap_birim_id, v_subject;
         CLOSE c_result_safahat;

         v_safahat_aciklama := v_subject;

         v_law_court_enf  := 'U';

         IF NVL(P_REASSIGN, 0) = 0 THEN
            v_law_file_no := alz_litigation_utils.match_uyap_law_file(v_uyap_file_id, v_uyap_birim_id, v_court_file_no);
         END IF;

         IF NVL(v_law_file_no, 'X') = 'X' THEN

             OPEN c_taraf_rolu (v_uyap_file_id, v_company_name);
             FETCH c_taraf_rolu INTO v_bre_lehte_aleyhte;
             CLOSE c_taraf_rolu;

             v_bre_lehte_aleyhte := NVL(v_bre_lehte_aleyhte, 'UNFAVORABLE');     -- UYAPDA AYIRT EDEMEZSE DEFAULT ALEYYH

             v_bre_department := GET_DEPARTMENT(v_company_name, v_law_court_enf, v_uyap_birim_id, v_bre_code);

         END IF;

         IF NVL(P_REASSIGN, 0) = 1 THEN
            IF NVL(v_law_file_no, 'X') = 'X' OR
               koc_law_utils.check_law_file(v_law_file_no) = 0
            THEN
                OPEN c_ticket_info_by_ticket_id(p_new_ticket_id);
               FETCH c_ticket_info_by_ticket_id INTO v_bre_department;
               CLOSE c_ticket_info_by_ticket_id;

               v_bre_lehte_aleyhte := 'UNFAVORABLE';
            ELSE
                OPEN c_law_file_info_by_lawfile_no(v_law_file_no);
               FETCH c_law_file_info_by_lawfile_no INTO v_law_file_info_by_lawfile_rec;
               CLOSE c_law_file_info_by_lawfile_no;

               v_bre_department := v_law_file_info_by_lawfile_rec.department;
               v_bre_lehte_aleyhte := v_law_file_info_by_lawfile_rec.favor;
            END IF;
         END IF;

      ELSIF p_source_type = 'UYAP_DOSYA' THEN

         v_ticket_type := 1;

         v_bre_source := 'DOSYA';
         v_source := 'UYAP';

         v_uyap_file_id := TO_NUMBER(p_param_id);
         v_company_name := p_subject;
         v_subject := 'UYAP'' dan yeni hukuk dosyasý alýndý.';

         OPEN  c_uyap_file_info(v_uyap_file_id, v_company_name);
         FETCH c_uyap_file_info INTO v_uyap_file_info_rec;
         CLOSE c_uyap_file_info;

         v_uyap_birim_id := v_uyap_file_info_rec.birim_id;
         v_court_file_no := v_uyap_file_info_rec.dosya_no;
         v_law_court_enf  := 'U';

         IF NVL(P_REASSIGN, 0) = 0 THEN
            v_law_file_no := alz_litigation_utils.match_uyap_law_file(v_uyap_file_id, v_uyap_birim_id, v_court_file_no);
         END IF;

         IF NVL(v_law_file_no, 'X') = 'X' THEN
            v_bre_department := GET_DEPARTMENT(v_company_name, v_law_court_enf, v_uyap_birim_id, 1);
         ELSE
            v_subject := 'UYAP'' dan yeni hukuk dosyasý alýndý ve '|| v_law_file_no ||' hukuk dosyasýyla eþleþtirildi.';
         END IF;

         IF NVL(P_REASSIGN, 0) = 1 THEN
            IF NVL(v_law_file_no, 'X') = 'X' OR
               koc_law_utils.check_law_file(v_law_file_no) = 0
            THEN
                OPEN c_ticket_info_by_ticket_id(p_new_ticket_id);
               FETCH c_ticket_info_by_ticket_id INTO v_bre_department;
               CLOSE c_ticket_info_by_ticket_id;

               v_bre_lehte_aleyhte := 'UNFAVORABLE';
            ELSE
                OPEN c_law_file_info_by_lawfile_no(v_law_file_no);
               FETCH c_law_file_info_by_lawfile_no INTO v_law_file_info_by_lawfile_rec;
               CLOSE c_law_file_info_by_lawfile_no;

               v_bre_department := v_law_file_info_by_lawfile_rec.department;
               v_bre_lehte_aleyhte := v_law_file_info_by_lawfile_rec.favor;
            END IF;
         END IF;

      ELSE

         RAISE e_wrong_source_type;

      END IF;

      IF p_source_type IN ('OTORIZASYON') THEN
        v_deadline := p_deadline;
      END IF;


      IF p_source_type IN ('MANUEL', 'BIRLESTIRME') THEN

        v_deadline := p_deadline;

      ELSE

          IF NVL(v_law_file_no, 'X') <> 'X' AND
             koc_law_utils.check_law_file(v_law_file_no) > 0
          THEN

             v_bre_is_matching := 'YES';

             OPEN c_law_file_info_by_lawfile_no(v_law_file_no);
             FETCH c_law_file_info_by_lawfile_no INTO v_law_file_info_by_lawfile_rec;
             CLOSE c_law_file_info_by_lawfile_no;

             v_bre_in_lawyer_user := v_law_file_info_by_lawfile_rec.in_lawyer_user;
             v_bre_in_lawyer_group := v_law_file_info_by_lawfile_rec.in_lawyer_group;
             v_bre_out_lawyer_user := v_law_file_info_by_lawfile_rec.out_lawyer_user;
             v_bre_out_lawyer_group := v_law_file_info_by_lawfile_rec.out_lawyer_group;
             v_bre_lehte_aleyhte := v_law_file_info_by_lawfile_rec.favor;
             v_bre_department := v_law_file_info_by_lawfile_rec.department;

          END IF;

          IF NVL(P_REASSIGN, 0) = 1 THEN
            IF NVL(v_law_file_no, 'X') = 'X' OR
               koc_law_utils.check_law_file(v_law_file_no) = 0
            THEN
                OPEN c_ticket_info_by_ticket_id(p_new_ticket_id);
               FETCH c_ticket_info_by_ticket_id INTO v_bre_department;
               CLOSE c_ticket_info_by_ticket_id;

               v_bre_lehte_aleyhte := 'UNFAVORABLE';
            ELSE
                OPEN c_law_file_info_by_lawfile_no(v_law_file_no);
               FETCH c_law_file_info_by_lawfile_no INTO v_law_file_info_by_lawfile_rec;
               CLOSE c_law_file_info_by_lawfile_no;

               v_bre_department := v_law_file_info_by_lawfile_rec.department;
               v_bre_lehte_aleyhte := v_law_file_info_by_lawfile_rec.favor;
            END IF;
         END IF;

          ALZ_LAW_TICKET_PROCESS_UTILS.GET_USER_STATUS(v_bre_in_lawyer_user, v_bre_in_lawyer_status);

          TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', 'yBEFORE-BRE_FIND_ASSINGMENT', v_bre_source || ' - ' || v_bre_is_matching || ' - ' || v_bre_code || ' - ' || v_bre_lehte_aleyhte || ' - ' || v_bre_department || ' - ' ||
                                         v_bre_in_lawyer_user || ' - ' || v_bre_in_lawyer_group || ' - ' || v_bre_in_lawyer_status || ' - ' || v_bre_out_lawyer_user || ' - ' || v_bre_out_lawyer_group || ' - ' || REPLACE(v_safahat_aciklama, 'I', 'I') || ' - ' ||
                                         v_bre_is_notification || ' - ' || v_bre_send_mail || ' - ' || v_bre_term || ' - ' || v_bre_is_ticket || ' - ' || v_bre_group || ' - ' ||
                                         v_bre_user || ' - ' || v_bre_message);


          IF p_source_type = 'OTORIZASYON' AND v_bre_in_lawyer_status = 'IZINDE' THEN
             OPEN c_law_file_info_by_lawfile_no(v_law_file_no);
             FETCH c_law_file_info_by_lawfile_no INTO v_law_file_info_by_lawfile_rec;
             CLOSE c_law_file_info_by_lawfile_no;

             v_bre_in_lawyer_group := v_law_file_info_by_lawfile_rec.in_lawyer_group;

             LITIGATION_BRE_CLIENT.FIND_ASSIGNED_USER('YES', v_bre_in_lawyer_group, v_bre_user,  v_bre_message);

          END IF;


          IF p_source_type NOT IN ('OTORIZASYON', 'DOSYA') THEN
              GET_BRE_PARAMETERS (p_source_type, p_reAssign, v_bre_source, v_bre_is_matching, v_bre_code, v_bre_lehte_aleyhte, v_bre_department,
                  v_bre_in_lawyer_user, v_bre_in_lawyer_group,  v_bre_in_lawyer_status, v_bre_out_lawyer_user, v_bre_out_lawyer_group, REPLACE(v_safahat_aciklama, 'I', 'I'),
                  v_bre_is_notification, v_bre_send_mail, v_bre_term, v_bre_is_ticket, v_bre_group, v_bre_user, v_bre_message );

                  IF v_bre_is_notification = 'YES' THEN

                     v_ticket_type := 1;
                     v_ticket_status := 'UNREAD';

                  ELSIF nvl( v_bre_is_ticket , 'NO' ) = 'NO' THEN

                     p_new_ticket_id := -1;
                     p_create_ticket_or_notif := 0;

                      alz_web_process_utils.process_result (
                      0,
                      9,
                      -1,
                      'CREATE_TICKET',
                      NVL(v_bre_message, 'Ýþ oluþturmayan bir evrak türü seçtiniz.'),
                      NVL(v_bre_message, 'Ýþ oluþturmayan bir evrak türü seçtiniz.'),
                      NULL,
                      NULL,
                      'ALZ_LAW_TICKET_PROCESS_UTILS.CREATE_TICKET',
                      NULL,
                      p_process_results);

                     RETURN;

                  END IF;

          END IF;

          TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', 'zAFTER-BRE_FIND_ASSINGMENT', v_bre_source || ' - ' || v_bre_is_matching || ' - ' || v_bre_code || ' - ' || v_bre_lehte_aleyhte || ' - ' || v_bre_department || ' - ' ||
                                         v_bre_in_lawyer_user || ' - ' || v_bre_in_lawyer_group || ' - ' || v_bre_in_lawyer_status || ' - ' || v_bre_out_lawyer_user || ' - ' || v_bre_out_lawyer_group || ' - ' || REPLACE(v_safahat_aciklama, 'I', 'I') || ' - ' ||
                                         v_bre_is_notification || ' - ' || v_bre_send_mail || ' - ' || v_bre_term || ' - ' || v_bre_is_ticket || ' - ' || v_bre_group || ' - ' ||
                                         v_bre_user || ' - ' || v_bre_message);

--          IF p_reAssign = 1 THEN
--            LITIGATION_BRE_CLIENT.FIND_ASSIGNED_USER('YES', v_bre_group, v_bre_user,  v_bre_message);
--
--            v_bre_in_lawyer_user := v_bre_user;
--          END IF;

          IF v_bre_send_mail = 'YES' THEN
             v_send_mail := 1;
          END IF;

          IF p_source_type NOT IN ('OTORIZASYON') THEN
            v_deadline := v_arrival_date + v_bre_term;
          END IF;

      END IF;

      IF NVL(p_reAssign, 0) = 0 THEN
        IF v_bre_is_notification = 'NO' AND v_bre_is_ticket = 'NO' THEN
            IF v_bre_message IS NOT NULL OR v_bre_user IS NULL THEN
                p_check_bre_param := 1;
                alz_web_process_utils.process_result (
                    0,
                    9,
                    -1,
                    'CREATE_TICKET',
                    NVL(v_bre_message, 'BRE den kullanýcý alýnamadý'),
                    NVL(v_bre_message, 'BRE den kullanýcý alýnamadý'),
                    NULL,
                    NULL,
                    'ALZ_LAW_TICKET_PROCESS_UTILS.CREATE_TICKET',
                    NULL,
                    p_process_results);
                p_create_ticket_or_notif := 0;
                RETURN;

            END IF;
        ELSE
--            p_new_ticket_id := -1;
--            p_create_ticket_or_notif := 0;
            RETURN;
        END IF;
      END IF;

      IF p_source_type = 'UYAP_EVRAK' THEN
           FOR related_ticket_list IN c_related_tickets(NVL(v_law_file_no, -1), NVL(v_uyap_file_id, -1), NVL(v_bre_code, -1))
           LOOP
               v_related_subject := v_related_subject || ': [' || related_ticket_list.TICKET_ID || ', ' || related_ticket_list.OWNER || ']';
           END LOOP related_ticket_list;

           IF LENGTH(v_related_subject) > 0 THEN
              v_subject := v_subject || ' Bu iþ ile eþleþmesi olasý diðer iþler' || v_related_subject;
           END IF;
      END IF;

   EXCEPTION
   WHEN e_wrong_source_type THEN
      alz_web_process_utils.process_result (
      0,
      9,
      -1,
      'CREATE_TICKET',
      'Geçersiz kaynak tipi.',
      'Geçersiz kaynak tipi.',
      NULL,
      NULL,
      'ALZ_LAW_TICKET_PROCESS_UTILS.CREATE_TICKET',
      NULL,
      p_process_results);

   WHEN e_group_validity THEN
      alz_web_process_utils.process_result (
      0,
      9,
      -1,
      'CREATE_TICKET',
      'Manuel atanan kiþi bir gruba kayýtlý deðil veya geçerlilik süresi dolu.',
      'Manuel atanan kiþi bir gruba kayýtlý deðil veya geçerlilik süresi dolu.',
      NULL,
      NULL,
      'ALZ_LAW_TICKET_PROCESS_UTILS.CREATE_TICKET',
      NULL,
      p_process_results);
   WHEN OTHERS THEN
      alz_web_process_utils.process_result (
      0,
      9,
      -1,
      'ORACLE_EXCEPTION',
      SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
      null,
      null,
      null,
      'ALZ_LAW_TICKET_PROCESS_UTILS.CREATE_TICKET',
      NULL,
      p_process_results);

   END;


PROCEDURE REDIRECT_TICKET(p_assigned_group       IN      VARCHAR2,
                          p_assigned_user        IN      VARCHAR2,
                          p_redirector           IN      VARCHAR2,
                          p_deadline             IN      DATE,
                          p_subject              IN      VARCHAR2,
                          p_redirect_reason_code IN      NUMBER,
                          p_ticket_id            IN      NUMBER,
                          p_process_results      OUT     customer.process_result_table)
   IS

   v_ticket_type               NUMBER          := NULL;
   v_old_owner_group           VARCHAR2(50)    := NULL;
   v_employee_id               VARCHAR2(30)    := NULL;
   v_subject                   VARCHAR2(4000)  := NULL;
   v_employee_id_assigned      VARCHAR2(30)    := NULL;
   v_old_owner                 VARCHAR2(20)    := NULL;
   v_ext_reference             VARCHAR2(30)    := NULL;
   v_department                VARCHAR2(20)    := NULL;

   v_bre_user                  VARCHAR2(20);
   v_bre_message               VARCHAR2(200);
   v_is_muhaberat              NUMBER          := 0;
   v_operation_time            DATE            := SYSDATE;

   v_call_invoke_rest          NUMBER          := 1;
   v_pr_err_count              NUMBER          := 0;


   CURSOR c_effected_tickets(v_ticket_id NUMBER)
      IS
         SELECT TICKET_ID
           FROM ALZ_LAW_TICKET
          WHERE TICKET_ID = v_ticket_id
             OR PARENT = v_ticket_id;

   BEGIN


     v_subject := p_subject;
     v_bre_user := p_assigned_user;


     SELECT TP.TICKET_TYPE_ID,
            TP.OLD_OWNER,
            TP.OLD_OWNER_GROUP,
            TP.DEPARTMENT,
            T.RUCU_FILE_NO
       INTO v_ticket_type,
            v_old_owner,
            v_old_owner_group,
            v_department,
            v_ext_reference
       FROM ALZ_LAW_TICKET_PROCESS TP, ALZ_LAW_TICKET T
      WHERE TP.TICKET_ID = T.TICKET_ID
        AND TP.TICKET_ID = p_ticket_id;


     IF p_assigned_group = 'LMMUNAUZ' THEN --Muhaberat Grubu
        SELECT CREATE_USER
          INTO v_bre_user
          FROM ALZ_LAW_TICKET_PROCESS
         WHERE TICKET_ID = p_ticket_id;

     ELSIF v_ticket_type = 4 AND p_assigned_group = 'LMRUNAUZ' THEN

        UPDATE ALZ_LAW_TICKET_PROCESS
           SET STATUS = 'CLOSED',
               CLOSE_DATE = SYSDATE,
               UPDATE_USER = p_redirector,
               UPDATE_DATE = v_operation_time
         WHERE TICKET_ID = p_ticket_id;

        UPDATE ALZ_LAW_TICKET
           SET SUBJECT = v_subject,
               UPDATE_USER = p_redirector,
               UPDATE_DATE = v_operation_time
         WHERE TICKET_ID = p_ticket_id;

        ALZ_LITIGATION_UTILS.REJECT_FILE (v_ext_reference, p_redirector, v_department, p_redirect_reason_code, p_process_results);

        begin
            v_pr_err_count := p_process_results.count;
        exception when others then
            v_pr_err_count := 0;
        end;

        if v_pr_err_count > 0 then
            if p_process_results(1).type = -1 then
                v_call_invoke_rest := 0;
                return;
            else
                v_call_invoke_rest := 1;
            end if;
        end if;

        ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_TICKET_HISTORY(p_ticket_id, 'Ilgili rücu dosyasý rücu birimine iade edildi ve is kapatildi.', p_redirector);

        v_employee_id_assigned := GET_EMPLOYEE_ID_BY_AZNET_NAME(v_bre_user);
        FOR ticket_ids in c_effected_tickets(p_ticket_id)
        LOOP
            IF v_call_invoke_rest = 1 THEN
                INVOKE_REST(null, 'http://esb.allianz.com.tr:12000/LitigationRest/GenericRestService/resources/myresource/REJECT/'||v_employee_id||'/'||ticket_ids.ticket_id||'/',p_process_results);
            END IF;
        END LOOP;

        RETURN;

     END IF;

     IF v_bre_user IS NULL THEN
        LITIGATION_BRE_CLIENT.find_assigned_user('YES', p_assigned_group, v_bre_user, v_bre_message);

        IF v_bre_message IS NOT NULL THEN

              alz_web_process_utils.process_result (
                  0,
                  9,
                  -1,
                  'REDIRECT_TICKET',
                  v_bre_message,
                  v_bre_message,
                  NULL,
                  NULL,
                  'ALZ_LAW_TICKET_PROCESS_UTILS.REDIRECT_TICKET',
                  NULL,
                  p_process_results);

              RETURN;

        END IF;

     END IF;

     begin
      select 1
        into v_is_muhaberat
        from alz_law_ticket
       where source = 'MUHABERAT'
         and ticket_id = p_ticket_id;
     exception when others then
          IF p_assigned_group = 'LMMUNAUZ' THEN --Muhaberat Grubu
             alz_web_process_utils.process_result (
                 0,
                 9,
                 -1,
                 'REDIRECT_TICKET',
                 'Muhaberat grubuna, yalnýzca Muhaberat kaynaklý iþler yönlendirilebilir.',
                 'Muhaberat grubuna, yalnýzca Muhaberat kaynaklý iþler yönlendirilebilir.',
                 NULL,
                 NULL,
                 'ALZ_LAW_TICKET_PROCESS_UTILS.REDIRECT_TICKET',
                 NULL,
                 p_process_results);

             RETURN;
          END IF;
     end;

     UPDATE ALZ_LAW_TICKET_PROCESS
        SET OLD_OWNER = OWNER,
            OLD_OWNER_GROUP = OWNER_GROUP,
            REDIRECTOR = p_redirector,
            OWNER_GROUP = p_assigned_group,
            OWNER = v_bre_user,
            RECEIVE_DATE = v_operation_time,
            REDIRECTION_REASON_CODE = p_redirect_reason_code,
            MUHABERAT_DEADLINE_DATE = p_deadline,
            UPDATE_USER = p_redirector,
            UPDATE_DATE = v_operation_time
      WHERE TICKET_ID = p_ticket_id
         OR TICKET_ID IN (SELECT TICKET_ID FROM ALZ_LAW_TICKET WHERE PARENT = p_ticket_id);

     UPDATE ALZ_LAW_TICKET
        SET SUBJECT = v_subject,
            UPDATE_USER = p_redirector,
            UPDATE_DATE = v_operation_time
      WHERE TICKET_ID = p_ticket_id
         OR PARENT = p_ticket_id;

     ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_TICKET_HISTORY(p_ticket_id, p_redirector || ' tarafindan, ' || v_bre_user || ' kullanýcýsina yönlendirildi. Not: ' || v_subject, p_redirector);

     v_employee_id := GET_EMPLOYEE_ID_BY_AZNET_NAME(v_old_owner);

     SELECT OLD_OWNER_GROUP
       INTO v_old_owner_group
       FROM ALZ_LAW_TICKET_PROCESS
      WHERE TICKET_ID = p_ticket_id;

     IF v_ticket_type = 4 THEN

        IF p_assigned_group = 'LMHA' THEN
            ALZ_LITIGATION_UTILS.SEND_FILE_TO_EXT_LAWYER(p_ticket_id, p_redirector, v_bre_user, p_process_results);

            begin
                v_pr_err_count := p_process_results.count;
            exception when others then
                v_pr_err_count := 0;
            end;

            if v_pr_err_count > 0 then
                if p_process_results(1).type = -1 then
                    v_call_invoke_rest := 0;
                    return;
                else
                    v_call_invoke_rest := 1;
                end if;
            end if;

            FOR ticket_ids in c_effected_tickets(p_ticket_id)
            LOOP
                IF v_call_invoke_rest = 1 THEN
                    INVOKE_REST(null,'http://esb.allianz.com.tr:12000/LitigationRest/GenericRestService/resources/myresource/OK/'||v_employee_id||'/'||ticket_ids.ticket_id||'/'||v_employee_id_assigned||'',p_process_results);
                END IF;
            END LOOP;

        ELSIF v_old_owner_group = 'LMHA' THEN
            ALZ_LITIGATION_UTILS.REJECT_FILE_BY_EXT_LAWYER (p_ticket_id, p_redirect_reason_code, p_process_results);

            begin
                v_pr_err_count := p_process_results.count;
            exception when others then
                v_pr_err_count := 0;
            end;

            if v_pr_err_count > 0 then
                if p_process_results(1).type = -1 then
                    v_call_invoke_rest := 0;
                    return;
                else
                    v_call_invoke_rest := 1;
                end if;
            end if;

            v_employee_id_assigned := GET_EMPLOYEE_ID_BY_AZNET_NAME(v_bre_user);
            FOR ticket_ids in c_effected_tickets(p_ticket_id)
            LOOP
                IF v_call_invoke_rest = 1 THEN
                    INVOKE_REST(null, 'http://esb.allianz.com.tr:12000/LitigationRest/GenericRestService/resources/myresource/REJECT/'||v_employee_id||'/'||ticket_ids.ticket_id||'/'||v_employee_id_assigned||'',p_process_results);
                END IF;
            END LOOP;

        END IF;
     ELSE
        IF v_ticket_type = 9 THEN
            IF v_old_owner_group = 'LMHA' THEN
                ALZ_LITIGATION_UTILS.REJECT_FILE_BY_EXT_LAWYER (p_ticket_id, p_redirect_reason_code, p_process_results);

                begin
                    v_pr_err_count := p_process_results.count;
                exception when others then
                    v_pr_err_count := 0;
                end;

                if v_pr_err_count > 0 then
                    if p_process_results(1).type = -1 then
                        v_call_invoke_rest := 0;
                        return;
                    else
                        v_call_invoke_rest := 1;
                    end if;
                end if;

                v_employee_id_assigned := GET_EMPLOYEE_ID_BY_AZNET_NAME(v_bre_user);
                FOR ticket_ids in c_effected_tickets(p_ticket_id)
                LOOP
                    IF v_call_invoke_rest = 1 THEN
                        INVOKE_REST(null, 'http://esb.allianz.com.tr:12000/LitigationRest/GenericRestService/resources/myresource/REJECT/'||v_employee_id||'/'||ticket_ids.ticket_id||'/'||v_employee_id_assigned||'',p_process_results);
                    END IF;
                END LOOP;

            END IF;
        END IF;

        v_employee_id_assigned := GET_EMPLOYEE_ID_BY_AZNET_NAME(v_bre_user);
        IF v_call_invoke_rest = 1 THEN
            INVOKE_REST(null, 'http://esb.allianz.com.tr:12000/LitigationRest/GenericRestService/resources/myresource/REJECT/'||v_employee_id||'/'||p_ticket_id||'/'||v_employee_id_assigned||'',p_process_results);
        END IF;
     END IF;


     IF p_assigned_group = 'LMMUNAUZ' THEN
       INVOKE_REST('<start><ticketId>'||p_ticket_id||'</ticketId><source>muhaberata</source><username>'||v_bre_user||'</username><deadline>'||TO_CHAR(p_deadline,'DD/MM/YYYY HH24:MI:SS')||'</deadline></start>', 'http://esb.allianz.com.tr:12000/LitigationRest/soa-infra/resources/default/LitigationProcessManagement/MuhaberatMailRestService/',p_process_results);
       INVOKE_REST('<start><ticketId>'||p_ticket_id||'</ticketId><deadline>'||TO_CHAR(p_deadline,'YYYY-MM-DD"T"HH24:MI:SS')||'</deadline><type>deadlineProcess</type><username>'||v_bre_user||'</username></start>', 'http://esb.allianz.com.tr:12000/LitigationRest/soa-infra/resources/default/LitigationProcessManagement/MuhaberatDeadlineRestService/',p_process_results);
     ELSIF v_old_owner_group = 'LMMUNAUZ' THEN
       INVOKE_REST('<start><ticketId>'||p_ticket_id||'</ticketId><source>muhaberattan</source><username>'||v_bre_user||'</username><deadline>'||TO_CHAR(p_deadline,'DD/MM/YYYY HH24:MI:SS')||'</deadline></start>', 'http://esb.allianz.com.tr:12000/LitigationRest/soa-infra/resources/default/LitigationProcessManagement/MuhaberatMailRestService/',p_process_results);
       INVOKE_REST(null, 'http://esb.allianz.com.tr:12000/LitigationRest/GenericRestService/resources/myresource/MUHABERATDEADLINE/'||v_employee_id||'/'||p_ticket_id||'/',p_process_results);
     END IF;

   EXCEPTION
   WHEN OTHERS THEN
      alz_web_process_utils.process_result (
      0,
      9,
      -1,
      'ORACLE_EXCEPTION',
      SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
      null,
      null,
      null,
      'ALZ_LAW_TICKET_PROCESS_UTILS.REDIRECT_TICKET',
      NULL,
      p_process_results);

   END;

   PROCEDURE REDIRECT_RELATED_TICKETS(p_assigned_group       IN      VARCHAR2,
                                      p_assigned_user        IN      VARCHAR2,
                                      p_redirector           IN      VARCHAR2,
                                      p_law_file_no          IN      VARCHAR2,
                                      p_process_results      OUT     CUSTOMER.PROCESS_RESULT_TABLE)
   IS


   CURSOR c_effected_tickets(v_law_file_no VARCHAR2, v_redirector VARCHAR2)
      IS
         SELECT A.TICKET_ID
           FROM ALZ_LAW_TICKET A, ALZ_LAW_TICKET_PROCESS B
          WHERE A.LAWFILE_NO = v_law_file_no
            AND B.OWNER = p_redirector
            AND B.STATUS IN ('OPEN', 'PAIRED', 'UNIFIED');

   BEGIN

        FOR related_ticket IN c_effected_tickets(p_law_file_no, p_redirector) LOOP

            ALZ_LAW_TICKET_PROCESS_UTILS.REDIRECT_TICKET(p_assigned_group,
                                                         p_assigned_user,
                                                         p_redirector,
                                                         null,
                                                         'Dahili avukat deðiþimi nedeniyle yönlendirildi.',
                                                         13, --Diðer
                                                         related_ticket.TICKET_ID,
                                                         p_process_results);

        END LOOP related_ticket;

   EXCEPTION
   WHEN OTHERS THEN
      alz_web_process_utils.process_result (
      0,
      9,
      -1,
      'ORACLE_EXCEPTION',
      SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
      null,
      null,
      null,
      'ALZ_LAW_TICKET_PROCESS_UTILS.REDIRECT_TICKET',
      NULL,
      p_process_results);

   END;

FUNCTION GET_UYAP_EVRAK_DEPARTMENT(p_dosya_id        IN     NUMBER,
                       p_evrak_id        IN     NUMBER,
                       p_gg_evrak_id     IN     NUMBER,
                       p_sirket_adi      IN     VARCHAR2) RETURN VARCHAR2
   IS

   v_department        VARCHAR2(30) := 'HDT';

   CURSOR c_department(v_dosya_id NUMBER, v_evrak_id NUMBER, v_gg_evrak_id NUMBER, v_sirket_adi VARCHAR2)
   IS
      SELECT GET_DEPARTMENT(DL.SIRKET_ADI, 'U', DL.BIRIM_ID, DT.CODE)
        FROM ALZ_UYAP_EVRAKLAR E, ALZ_UYAP_DOSYA_LISTESI DL, ALZ_LAW_DOC_TYPE_DEF DT
       WHERE E.DOSYA_ID = DL.DOSYA_ID
         AND E.SIRKET_ADI = DL.SIRKET_ADI
         AND REPLACE(LOWER(E.TURU), CHR(10),' ') = LOWER(DT.DOC_DESC)
         AND E.DOSYA_ID = v_dosya_id
         AND E.EVRAK_ID = v_evrak_id
         AND E.GG_EVRAK_ID = v_gg_evrak_id
         AND E.SIRKET_ADI = v_sirket_adi;

   BEGIN

      OPEN c_department(p_dosya_id, p_evrak_id, p_gg_evrak_id, p_sirket_adi);
      FETCH c_department INTO v_department;
      CLOSE c_department;

      RETURN v_department;

   END;

FUNCTION GET_DEPARTMENT(p_company_name         IN     VARCHAR2,
                       p_court_enf_type        IN     VARCHAR2,
                       p_court_enf_no          IN     VARCHAR2,
                       p_code                  IN     NUMBER) RETURN VARCHAR2
   IS

   v_court_type        NUMBER       := NULL;
   v_court_enf_type    VARCHAR2(1)  := NULL;

   CURSOR c_court_type
   IS
   SELECT K.COURT_TYPE
     FROM KOC_LAW_COURT_REF K
    WHERE K.COURT_ENF_NO = TO_NUMBER(p_court_enf_no)
      AND K.COURT_ENF_TYPE = p_court_enf_type
      AND K.VALIDITY_START_DATE <= SYSDATE
      AND (K.VALIDITY_END_DATE >= SYSDATE OR K.VALIDITY_END_DATE IS NULL);

   CURSOR c_court_type_uyap
   IS
   SELECT K.COURT_ENF_TYPE, K.COURT_TYPE
     FROM KOC_LAW_COURT_REF K, ALZ_UYAP_COURT_REF U
    WHERE K.COURT_ENF_NO = U.COURT_ENF_NO
      AND K.COURT_ENF_TYPE = U.COURT_ENF_TYPE
      AND U.BIRIM_ID = p_court_enf_no
      AND K.VALIDITY_START_DATE <= SYSDATE
      AND (K.VALIDITY_END_DATE >= SYSDATE OR K.VALIDITY_END_DATE IS NULL);


   BEGIN

      IF p_company_name IN ('AZYE', 'AZHE', 'MAGDEBURGER', 'BEYKOZ', 'AZS-AS410') THEN
         RETURN 'KDT';
      END IF;

      IF p_court_enf_type = 'U' THEN

         OPEN c_court_type_uyap;
         LOOP

            FETCH c_court_type_uyap INTO v_court_enf_type, v_court_type;
            EXIT WHEN c_court_type_uyap%NOTFOUND;

            IF v_court_enf_type = 'S' THEN
               CLOSE c_court_type_uyap;
               RETURN 'KDT';
            ELSIF (v_court_enf_type = 'M' AND v_court_type IN (5, 11, 12, 13, 14)) OR (v_court_enf_type = 'Y' and v_court_type = 2) THEN
               CLOSE c_court_type_uyap;
               RETURN 'KDT';
            END IF;

         END LOOP;
         CLOSE c_court_type_uyap;

      ELSIF p_court_enf_type = 'S' THEN
         RETURN 'KDT';
      ELSIF p_court_enf_type IN ('M', 'Y') THEN

         OPEN c_court_type;
         FETCH c_court_type INTO v_court_type;
         CLOSE c_court_type;

         IF (p_court_enf_type = 'M' AND v_court_type IN (5, 11, 12, 13, 14)) OR (p_court_enf_type = 'Y' and v_court_type = 2) THEN
            CLOSE c_court_type;
            RETURN 'KDT';
         END IF;

      END IF;

      RETURN 'HDT';

   END;

PROCEDURE UPSERT_LAW_DOC_TYPE_DEF     (p_doc_name              IN   VARCHAR2,
                                       p_doc_type              IN   VARCHAR2,
                                       p_doc_source            IN   VARCHAR2,
                                       p_doc_code              IN   NUMBER,
                                       p_process_results       OUT  customer.process_result_table)

IS
BEGIN

    IF NVL(p_doc_code, 0) = 0 THEN
        INSERT INTO ALZ_LAW_DOC_TYPE_DEF (CODE, SOURCE, DOC_TYPE, DOC_DESC)
        VALUES (ALZ_LAW_DOC_TYPE_SEQ.NEXTVAL, p_doc_source, p_doc_type, p_doc_name );
    ELSE
       UPDATE ALZ_LAW_DOC_TYPE_DEF
       SET SOURCE = p_doc_source, DOC_TYPE = p_doc_type, DOC_DESC = p_doc_name
       WHERE CODE = p_doc_code;
    END IF;

EXCEPTION
    WHEN OTHERS THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
           'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
            'ALZ_LAW_TICKET_PROCESS_UTILS.UPSERT_LAW_DOC_TYPE_DEF',
            NULL,
            p_process_results);

END;

PROCEDURE GET_GROUPS     (p_group_id            IN      VARCHAR2,
                          p_parent              IN      VARCHAR2,
                          p_group_name          IN      VARCHAR2,
                          p_assignable          IN      NUMBER,
                          cur                   OUT     refcur)

IS
  v_dynamic_sql           varchar2(4000);

 BEGIN

    v_dynamic_sql:= ' SELECT A.CODE , '
                    || 'A.PARENT , '
                    || 'A.DEFINITION , '
                    || 'A.ASSIGNABLE , '
                    || 'A.VALIDITY_START_DATE , '
                    || 'A.VALIDITY_END_DATE , '
                    || 'B.DEFINITION AS PARENT_DEF '
                    || 'FROM ALZ_LAW_GROUP_DEF A '
                    || 'LEFT JOIN ALZ_LAW_GROUP_DEF B ON A.PARENT = B.CODE '
                    || 'WHERE 1=1 '
                    || 'AND (TRUNC(A.VALIDITY_END_DATE) IS NULL '
                    || 'OR TRUNC(A.VALIDITY_END_DATE)>=TRUNC(SYSDATE)) ';

    IF p_group_id IS NOT NULL THEN
        v_dynamic_sql := v_dynamic_sql || 'AND A.CODE = ''' || p_group_id || ''' ';
    END IF;

    IF p_group_name IS NOT NULL THEN
        v_dynamic_sql := v_dynamic_sql || 'AND UPPER(A.DEFINITION) LIKE UPPER(''%' || p_group_name || '%'') ';
    END IF;

    IF p_parent IS NOT NULL THEN
        v_dynamic_sql := v_dynamic_sql || 'AND A.PARENT = ''' || p_parent || ''' ';
    END IF;

    IF p_assignable = 0 OR p_assignable = 1 THEN
        v_dynamic_sql := v_dynamic_sql || 'AND A.ASSIGNABLE = ''' || p_assignable || ''' ';
    END IF;

    OPEN cur FOR v_dynamic_sql;

END;

PROCEDURE UPDATE_GROUP_INFO (p_group_id             IN      VARCHAR2,
                             p_parent               IN      VARCHAR2,
                             p_group_name           IN      VARCHAR2,
                             p_assignable           IN      NUMBER,
                             p_validity_start_date  IN      DATE,
                             p_validity_end_date    IN      DATE)
IS
BEGIN

     UPDATE   ALZ_LAW_GROUP_DEF
     SET CODE       = p_group_id,
         PARENT     = p_parent,
         ASSIGNABLE = p_assignable,
         DEFINITION = p_group_name
         WHERE     VALIDITY_START_DATE = p_validity_start_date
         AND VALIDITY_END_DATE   = p_validity_end_date
         AND CODE = p_group_id ;
END;

PROCEDURE INSERT_ALZ_GROUP_INFO (p_group_id               IN      VARCHAR2,
                                 p_parent                 IN      VARCHAR2,
                                 p_group_name             IN      VARCHAR2,
                                 p_validity_start_date    IN      DATE,
                                 p_validity_end_date      IN      DATE,
                                 p_assignable             IN      NUMBER,
                                 p_process_results        OUT   customer.process_result_table)
   IS

   CURSOR CUR_IF_NULL IS
   SELECT CODE , VALIDITY_END_DATE
   FROM ALZ_LAW_GROUP_DEF
   WHERE CODE = p_group_id
   AND VALIDITY_START_DATE < SYSDATE
   AND VALIDITY_END_DATE IS NULL;

   v_result                 NUMBER;
   V_CODE                   VARCHAR2(20);
   V_VALIDITY_END_DATE      DATE;

BEGIN

       IF p_group_id IS NULL OR p_validity_start_date IS NULL
       THEN
          alz_web_process_utils.process_result (
             0,
             9,
             -1,
             'INSERT_ALZ_GROUP_INFO',
             'Grup kodunu ve baþlangýç tarihini boþ geçemezsiniz!',
             'Grup kodunu ve baþlangýç tarihini boþ geçemezsiniz!',
             NULL,
             NULL,
             'ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_ALZ_GROUP_INFO',
             NULL,
             p_process_results
          );
       END IF;

       BEGIN

            SELECT COUNT(*) INTO v_result
            FROM ALZ_LAW_GROUP_DEF
            WHERE CODE = p_group_id
            AND (TRUNC(p_validity_start_date) BETWEEN TRUNC(VALIDITY_START_DATE+1) AND TRUNC(NVL(VALIDITY_END_DATE,to_date('10/29/9999', 'MM/DD/YY'))) OR TRUNC(NVL(p_validity_end_date,to_date('10/29/9999', 'MM/DD/YY'))) BETWEEN TRUNC(VALIDITY_START_DATE+1)AND  TRUNC(NVL(VALIDITY_END_DATE,to_date('10/29/9999', 'MM/DD/YY')))
            OR (TRUNC(p_validity_start_date)<TRUNC(VALIDITY_START_DATE) AND TRUNC(NVL(p_validity_end_date,to_date('10/29/9999', 'MM/DD/YY'))) >= TRUNC(NVL(VALIDITY_END_DATE,to_date('10/29/9999', 'MM/DD/YY')))  ));

       IF v_result <> 0 THEN
          alz_web_process_utils.process_result (
             0,
             9,
             -1,
             'INSERT_ALZ_GROUP_INFO',
             'Þu an bu grup kodu için geçerli bir taným bulunmaktadýr!',
             'Þu an bu grup kodu için geçerli bir taným bulunmaktadýr!',
             NULL,
             NULL,
             'ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_ALZ_GROUP_INFO',
             NULL,
             p_process_results
          );
       END IF;
       END;


     IF p_process_results is null or p_process_results.COUNT = 0
       THEN

       OPEN CUR_IF_NULL;
       LOOP
           FETCH CUR_IF_NULL INTO V_CODE, V_VALIDITY_END_DATE;
           EXIT WHEN CUR_IF_NULL%NOTFOUND;

               UPDATE ALZ_LAW_GROUP_DEF SET VALIDITY_END_DATE = SYSDATE
               WHERE CODE = p_group_id AND VALIDITY_END_DATE IS NULL;

       END LOOP;
       CLOSE CUR_IF_NULL;

             INSERT INTO ALZ_LAW_GROUP_DEF (CODE, PARENT, ASSIGNABLE , DEFINITION, VALIDITY_START_DATE, VALIDITY_END_DATE)
             VALUES (p_group_id,p_parent,p_assignable, p_group_name, p_validity_start_date, p_validity_end_date);

        END IF;

   EXCEPTION

     WHEN  DUP_VAL_ON_INDEX
     THEN
        alz_web_process_utils.process_result (
           0,
           9,
           -1,
           'INVALID_DATA',
           'Bu grup daha önce tanýmlanmýþ bulunmaktadýr!',
           'Bu grup daha önce tanýmlanmýþ bulunmaktadýr!',
           NULL,
           NULL,
           'ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_ALZ_GROUP_INFO',
           NULL,
           p_process_results
        );

     WHEN OTHERS
     THEN
        alz_web_process_utils.process_result (
           0,
           9,
           -1,
           'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
           'ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_ALZ_GROUP_INFO',
           NULL,
           p_process_results
        );
END;

PROCEDURE GET_USERS    (p_group_code          IN   VARCHAR2,
                        p_user_name            IN   VARCHAR2,
                        cur                   OUT   refcur)

IS
    v_dynamic_sql       varchar2(4000);

BEGIN

    v_dynamic_sql:=' SELECT A.GROUP_CODE , '
                || 'A.USERNAME , '
                || 'A.VALIDITY_START_DATE , '
                || 'A.VALIDITY_END_DATE , '
                || 'B.DEFINITION '
                || 'FROM ALZ_LAW_GROUP_USER_REL A '
                || 'LEFT JOIN ALZ_LAW_GROUP_DEF B ON A.GROUP_CODE = B.CODE '
                || 'WHERE 1=1 '
                || 'AND (TRUNC(A.VALIDITY_END_DATE) IS NULL '
                || 'OR TRUNC(A.VALIDITY_END_DATE)>=TRUNC(SYSDATE)) ';


    IF p_group_code IS NOT NULL THEN
        v_dynamic_sql := v_dynamic_sql || 'AND GROUP_CODE = ''' || p_group_code || ''' ';
    END IF;

    IF p_user_name IS NOT NULL THEN
        v_dynamic_sql := v_dynamic_sql || 'AND USERNAME = ''' || p_user_name || ''' ';
    END IF;

    OPEN cur FOR v_dynamic_sql;

END;

PROCEDURE INSERT_USER  (p_group_code          IN   VARCHAR2,
                        p_user_name            IN   VARCHAR2,
                        p_validity_start_date IN   DATE,
                        p_validity_end_date   IN   DATE,
                        p_process_results     OUT   customer.process_result_table)

IS

   v_result                 NUMBER;

BEGIN

   SELECT COUNT (*) INTO v_result
   FROM ALZ_LAW_GROUP_USER_REL
   WHERE USERNAME = p_user_name
   AND (TRUNC(p_validity_start_date) BETWEEN TRUNC(VALIDITY_START_DATE+1) AND TRUNC(NVL(VALIDITY_END_DATE,to_date('10/29/9999', 'MM/DD/YY'))) OR TRUNC(NVL(p_validity_end_date,to_date('10/29/9999', 'MM/DD/YY'))) BETWEEN TRUNC(VALIDITY_START_DATE+1)AND  TRUNC(NVL(VALIDITY_END_DATE,to_date('10/29/9999', 'MM/DD/YY')))
   OR (TRUNC(p_validity_start_date)<TRUNC(VALIDITY_START_DATE) AND TRUNC(NVL(p_validity_end_date,to_date('10/29/9999', 'MM/DD/YY'))) >= TRUNC(NVL(VALIDITY_END_DATE,to_date('10/29/9999', 'MM/DD/YY')))  ));


   IF p_group_code IS NULL OR p_validity_start_date IS NULL OR p_user_name IS NULL
   THEN
      alz_web_process_utils.process_result (
         0,
         9,
         -1,
         'INSERT_USER',
         'Grup kodunu, kullanýcý adýný ve baþlangýç tarihini boþ geçemezsiniz!',
         'Grup kodunu, kullanýcý adýný ve baþlangýç tarihini boþ geçemezsiniz!',
         NULL,
         NULL,
         'ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_USER',
         NULL,
         p_process_results
      );
   END IF;


   IF v_result <> 0 THEN
      alz_web_process_utils.process_result (
         0,
         9,
         -1,
         'INSERT_USER',
         'Þu an bu kullanýcý adý için geçerli bir taným bulunmaktadýr!',
         'Þu an bu kullanýcý adý için geçerli bir taným bulunmaktadýr!',
         NULL,
         NULL,
         'ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_USER',
         NULL,
         p_process_results
      );

    ELSE

        INSERT INTO ALZ_LAW_GROUP_USER_REL (GROUP_CODE,USERNAME,VALIDITY_START_DATE,VALIDITY_END_DATE)
        VALUES ( p_group_code,p_user_name, p_validity_start_date, p_validity_end_date );

    END IF;

EXCEPTION

    WHEN DUP_VAL_ON_INDEX THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
            'INVALID_DATA',
            'Bu kullanýcý daha önce tanýmlanmýþ bulunmaktadýr!',
            'Bu kullanýcý daha önce tanýmlanmýþ bulunmaktadýr!',
            NULL,
            NULL,
            'ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_USER',
            NULL,
            p_process_results
        );

    WHEN OTHERS THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
           'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
            'ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_USER',
            NULL,
            p_process_results
        );
END;

PROCEDURE UPDATE_USER       (p_group_code           IN      VARCHAR2,
                             p_user_name             IN      VARCHAR2,
                             p_validity_start_date  IN      DATE,
                             p_validity_end_date    IN      DATE,
                             p_oldgroup_code        IN      VARCHAR2,
                             p_process_results      OUT  customer.process_result_table)
IS
    v_result number;

BEGIN
       SELECT COUNT (*) INTO v_result
       FROM ALZ_LAW_GROUP_USER_REL
       WHERE USERNAME = p_user_name
       AND (TRUNC(p_validity_start_date) BETWEEN TRUNC(VALIDITY_START_DATE+1) AND TRUNC(NVL(VALIDITY_END_DATE,to_date('10/29/9999', 'MM/DD/YY'))) OR TRUNC(NVL(p_validity_end_date,to_date('10/29/9999', 'MM/DD/YY'))) BETWEEN TRUNC(VALIDITY_START_DATE+1)AND  TRUNC(NVL(VALIDITY_END_DATE,to_date('10/29/9999', 'MM/DD/YY')))
       OR (TRUNC(p_validity_start_date)<TRUNC(VALIDITY_START_DATE) AND TRUNC(NVL(p_validity_end_date,to_date('10/29/9999', 'MM/DD/YY'))) >= TRUNC(NVL(VALIDITY_END_DATE,to_date('10/29/9999', 'MM/DD/YY')))  ));


     IF p_group_code <> p_oldgroup_code THEN

     UPDATE ALZ_LAW_GROUP_USER_REL
     SET GROUP_CODE = p_oldgroup_code, USERNAME = p_user_name ,VALIDITY_START_DATE = trunc(p_validity_start_date),VALIDITY_END_DATE =trunc(SYSDATE-1)
     WHERE GROUP_CODE = p_oldgroup_code AND USERNAME = p_user_name;

     IF ( v_result <> 0 ) THEN

        UPDATE ALZ_LAW_GROUP_USER_REL
        SET VALIDITY_START_DATE = trunc(SYSDATE-1),VALIDITY_END_DATE = NULL
        WHERE GROUP_CODE = p_group_code AND USERNAME = p_user_name;

        alz_web_process_utils.process_result (
                        0,
                        9,
                        -1,
                        'UPDATE_USER',
                        'Grup ',
                        'Kullanýcý grup kaydý yapýlamadý!',
                        NULL,
                        NULL,
                        'ALZ_LAW_TICKET_PROCESS_UTILS.GET_ALL_USERS',
                        NULL,
                        p_process_results);


     ELSE

        INSERT INTO ALZ_LAW_GROUP_USER_REL (GROUP_CODE, USERNAME, VALIDITY_START_DATE, VALIDITY_END_DATE)
        VALUES (p_group_code,p_user_name, TRUNC(SYSDATE), NULL);



     END IF;


     END IF;

     EXCEPTION
       WHEN OTHERS
       THEN
         alz_web_process_utils.process_result (
            0,
            9,
            -1,
           'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
            'ALZ_LAW_TICKET_PROCESS_UTILS.UPDATE_USER',
            NULL,
            p_process_results);
END;

PROCEDURE GET_COURT_INFO (p_law_file_no                   IN      VARCHAR2,
                          cur                             OUT     refcur)
   IS
   BEGIN

    OPEN cur FOR
      SELECT * FROM (
                    SELECT
                        A.LAW_FILE_NO,
                        B.COURT_FILE_NO,
                        B.COURT_ID,
                        B.COURT_TYPE,
                        C.COURT_NAME
                      FROM
                        KOC_LAW_BASES A ,
                        KOC_LAW_BASE_COURTS B ,
                        ALZ_LAW_COURTS_V C
                      WHERE
                        A.LAW_FILE_NO = p_law_file_no
                        AND A.LAW_FILE_NO = B.LAW_FILE_NO (+)
                        AND B.COURT_ID = C.COURT_ID (+)
                        AND B.COURT_TYPE = C.COURT_TYPE (+)
                      ORDER BY DECODE(B.COURT_ENF_TYPE, 'M', 1, 'I', 2, 'Y', 3, 'T', 4, 'S', 5, 6)
                   ) WHERE ROWNUM=1;
   END;

PROCEDURE GET_LAW_FILE (p_law_file_no                   IN      VARCHAR2,
                        p_lawyer_part_id                IN      NUMBER,
                        p_ext_reference                 IN      VARCHAR2,
                        p_recourse_included             IN      NUMBER,
                        p_file_status                   IN      NUMBER,
                        p_file_open_date_start          IN      VARCHAR2,
                        p_file_open_date_end            IN      VARCHAR2,
                        p_file_close_date_start         IN      VARCHAR2,
                        p_file_close_date_end           IN      VARCHAR2,
                        p_defendant_identity_no         IN      VARCHAR2,
                        p_defendant_first_name          IN      VARCHAR2,
                        p_defendant_surname             IN      VARCHAR2,
                        p_plaintiff_identity_no         IN      VARCHAR2,
                        p_plaintiff_first_name          IN      VARCHAR2,
                        p_plaintiff_surname             IN      VARCHAR2,
                        p_other_party_identity_no       IN      VARCHAR2,
                        p_other_party_first_name        IN      VARCHAR2,
                        p_other_party_surname           IN      VARCHAR2,
                        p_opponent_identity_no          IN      VARCHAR2,
                        p_in_lawyer                     IN      VARCHAR2,
                        p_policy_ref                    IN      VARCHAR2,
                        p_plate_no                      IN      VARCHAR2,
                        p_agency_id                     IN      NUMBER,
                        p_court_date_start              IN      VARCHAR2,
                        p_court_date_end                IN      VARCHAR2,
                        p_claim_date_start              IN      VARCHAR2,
                        p_claim_date_end                IN      VARCHAR2,
                        p_compesation_pay_date_start    IN      VARCHAR2,
                        p_compesation_pay_date_end      IN      VARCHAR2,
                        p_court_enf_no                  IN      VARCHAR2,
                        p_court_enf_type                IN      VARCHAR2,
                        p_court_file_info               IN      VARCHAR2,
                        p_all_lawyers                   IN      NUMBER,
                        p_lawyer_reference              IN      VARCHAR2,
                        p_user_name                     IN      VARCHAR2,
                        cur                             OUT     refcur,
                        p_process_results               OUT     customer.process_result_table)
   IS
      v_dynamic_sql varchar2(20000);
      v_is_inlawyer  NUMBER;
      v_is_outlawyer NUMBER;

      CURSOR has_user_inlawyer_role(
         p_user_name VARCHAR2
      )
      IS
        SELECT COUNT(*)
          FROM KOC_AUTH_USER_ROLE_REL
         WHERE USERNAME = p_user_name
           AND (ROLE_CODE <> 'LMRHA' AND ROLE_CODE <> 'LMRMH' AND ROLE_CODE <> 'LMRTP')
           AND TRUNC(VALIDITY_START_DATE) <= TRUNC(SYSDATE)
           AND (TRUNC(VALIDITY_END_DATE) >= SYSDATE OR VALIDITY_END_DATE IS NULL);

      CURSOR has_user_outlawyer_role(
         p_user_name VARCHAR2
      )
      IS
        SELECT COUNT(*)
          FROM KOC_AUTH_USER_ROLE_REL
         WHERE USERNAME = p_user_name
           AND (ROLE_CODE = 'LMRHA')
           AND TRUNC(VALIDITY_START_DATE) <= TRUNC(SYSDATE)
           AND (TRUNC(VALIDITY_END_DATE) >= SYSDATE OR VALIDITY_END_DATE IS NULL);

   BEGIN

      IF p_all_lawyers = 1 THEN
         v_is_inlawyer := 1;
      ELSE
          OPEN has_user_inlawyer_role (p_user_name);
         FETCH has_user_inlawyer_role INTO v_is_inlawyer;
         CLOSE has_user_inlawyer_role;
      END IF;

       OPEN has_user_outlawyer_role (p_user_name);
      FETCH has_user_outlawyer_role INTO v_is_outlawyer;
      CLOSE has_user_outlawyer_role;

      v_dynamic_sql:=      'SELECT '
                        || '      H.LAW_FILE_NO, '
                        || '      UTR.TARAF_ROL_KODU RES_DEF_TYPE, '
                        || '      UTR.TARAF_ROL_ACIKLAMA, '
                        || '      DECODE(PARTNER_TYPE,''I'',NAME,''P'',FIRST_NAME||'' ''||NAME) NAME , '
                        || '      DECODE(PARTNER_TYPE,''I'',TAX_NUMBER,''P'',IDENTITY_NO) IDENTITY_NO , '
                        || '      K.NAME_SURNAME, '
                        || '      DECODE(H.SUPPLIER_TYPE, ''HM'', ''KDT'', NVL(H.SUPPLIER_TYPE, ''HDT'')) SUPPLIER_TYPE, '
                        || '      R.EXPLANATION FILE_STATUS, '
                        || '      T.EXPLANATION FILE_TYPE, '
                        || '      H.FILE_OPEN_DATE, '
                        || '      H.FILE_CLOSE_DATE, '
                        || '      S.COURT_FILE_NO, '
                        || '      S.COURT_ID, '
                        || '      S.COURT_TYPE, '
                        || '      S.COURT_NAME, '
                        ||  '     H.LAWYER_REFERENCE '
                        || ' FROM '
                        || '      KOC_LAW_BASES H ,'
                        || '      KOC_LAW_INTERESTED_PARTIES E, '
                        || '      ALZ_UYAP_TARAF_ROLLERI UTR, ';

      IF p_policy_ref IS NOT NULL OR p_plate_no IS NOT NULL OR p_ext_reference IS NOT NULL OR (p_compesation_pay_date_start IS NOT NULL AND p_compesation_pay_date_end IS NOT NULL)
        OR (p_claim_date_start IS NOT NULL AND p_claim_date_end IS NOT NULL) THEN
         v_dynamic_sql := v_dynamic_sql
                        || '      KOC_LAW_BASES_DETAIL A, ';
      END IF;

      v_dynamic_sql := v_dynamic_sql
                        || '      CP_PARTNERS F ,'
                        || '      KOC_CP_PARTNERS_EXT G ,'
                        || '      ALZ_LAWYERS K, '
                        || '      KOC_LAW_STATUS_REF R, '
                        || '      KOC_LAW_SF_TYPE T, '
                        || '      ( '
                        || '      SELECT * '
                        || '        FROM (  SELECT B.LAW_FILE_NO, '
                        || '                       B.COURT_FILE_NO, '
                        || '                       B.COURT_ID, '
                        || '                       B.COURT_TYPE, '
                        || '                       C.COURT_NAME, '
                        || '                       ROW_NUMBER() OVER (PARTITION BY B.LAW_FILE_NO '
                        || '                 ORDER BY DECODE (b.court_enf_type, ''M'', 1, ''I'', 2, ''Y'', 3, ''T'', 4, ''S'', 5, 6)) AS ROW_NUMBER '
                        || '                  FROM KOC_LAW_BASE_COURTS B, ALZ_LAW_COURTS_V C '
                        || '                 WHERE B.COURT_ID = C.COURT_ID(+) '
                        || '                   AND B.COURT_TYPE = C.COURT_TYPE(+) ';

      IF p_court_enf_no IS NOT NULL AND p_court_enf_type IS NOT NULL THEN
         v_dynamic_sql := v_dynamic_sql
                        || '                   AND B.COURT_TYPE = ''' || p_court_enf_type || ''' AND B.COURT_ID = ''' || p_court_enf_no || ''' ';
      END IF;

      IF p_court_file_info IS NOT NULL THEN
         v_dynamic_sql := v_dynamic_sql
                        || '                   AND B.COURT_FILE_NO = ''' || p_court_file_info || ''' ';
      END IF;

         v_dynamic_sql := v_dynamic_sql
                        || '       ORDER BY DECODE (b.court_enf_type, ''M'', 1, ''I'', 2, ''Y'', 3, ''T'', 4, ''S'', 5, 6)) '
                        || '        WHERE ROW_NUMBER = 1 '
                        || '       ) S '
                        || 'WHERE '
                        || '      H.LAW_FILE_NO = E.LAW_FILE_NO (+) '
                        || '  AND  decode(E.res_def_type, ''R'', ''1'', ''D'', ''2'', ''A'', ''3'', ''C'', ''7'', E.res_def_type) = to_char(UTR.taraf_rol_kodu (+)) ';


      IF p_policy_ref IS NOT NULL OR p_plate_no IS NOT NULL OR p_ext_reference IS NOT NULL OR (p_compesation_pay_date_start IS NOT NULL AND p_compesation_pay_date_end IS NOT NULL)
        OR (p_claim_date_start IS NOT NULL AND p_claim_date_end IS NOT NULL) THEN
         v_dynamic_sql := v_dynamic_sql
                        || '  AND H.LAW_FILE_NO = A.LAW_FILE_NO ';
      END IF;

      v_dynamic_sql := v_dynamic_sql
                        || '  AND E.PART_ID = F.PART_ID (+) '
                        || '  AND E.PART_ID = G.PART_ID (+) '
                        || '  AND H.IN_LAWYER = K.ID (+) '
                        || '  AND H.STATUS_ID = R.STATUS_ID (+) '
                        || '  AND H.LAW_SF_TYPE = T.LAW_SF_TYPE (+) '
                        || '  AND H.LAW_FILE_NO = S.LAW_FILE_NO(+) ';


      IF p_policy_ref IS NOT NULL THEN
         v_dynamic_sql := v_dynamic_sql || ' AND EXISTS ( '
                                        || '    SELECT 1 FROM CLM_POL_BASES B '
                                        || '      WHERE A.CLAIM_ID = B.CLAIM_ID '
                                        || '        AND B.POLICY_REF =  ''' || p_policy_ref || ''') ';
      END IF;

      IF p_plate_no IS NOT NULL THEN
         v_dynamic_sql := v_dynamic_sql || ' AND EXISTS ( '
                                        || '    SELECT 1 FROM KOC_CLM_VEH_INFO V '
                                        || '      WHERE A.CLAIM_ID = V.CLAIM_ID '
                                        || '        AND V.PLATE_NO = ''' || p_plate_no || ''') ';
      END IF;

      IF p_ext_reference IS NOT NULL THEN
         IF NVL(p_recourse_included, 0) != 0 THEN

            v_dynamic_sql := v_dynamic_sql || ' AND (EXISTS ( '
                                           || '    SELECT 1 FROM KOC_CLM_RECOURSE_DETAIL D '
                                           || '      WHERE A.CLAIM_ID = D.CLAIM_ID '
                                           || '        AND A.SF_NO = D.SF_NO '
                                           || '        AND D.CLM_EXT_REFERENCE = ''' || p_ext_reference || ''') '
                                           || ' OR EXISTS ( '
                                           || '    SELECT 1 FROM CLM_SUBFILES S '
                                           || '      WHERE A.CLAIM_ID = S.CLAIM_ID '
                                           || '        AND A.SF_NO = S.SF_NO '
                                           || '        AND S.EXT_REFERENCE = ''' || p_ext_reference || ''') ) ';
         ELSE
            v_dynamic_sql := v_dynamic_sql || ' AND EXISTS ( '
                                           || '    SELECT 1 FROM CLM_SUBFILES S '
                                           || '      WHERE A.CLAIM_ID = S.CLAIM_ID '
                                           || '        AND A.SF_NO = S.SF_NO '
                                           || '        AND S.EXT_REFERENCE = ''' || p_ext_reference || ''') ';
         END IF;
      END IF;

      IF p_compesation_pay_date_start IS NOT NULL AND p_compesation_pay_date_end IS NOT NULL THEN
         v_dynamic_sql := v_dynamic_sql || ' AND EXISTS ( '
                                        || '    SELECT 1 FROM KOC_CLM_V_TRANS C '
                                        || '      WHERE A.CLAIM_ID = C.CLAIM_ID '
                                        || '        AND C.PAYMENT_DATE BETWEEN TO_DATE(''' || p_compesation_pay_date_start ||''',''dd/MM/yyyy'')' || ' AND TO_DATE(''' || p_compesation_pay_date_end  || ''',''dd/MM/yyyy'')) ';
      END IF;

      IF p_claim_date_start IS NOT NULL AND p_claim_date_end IS NOT NULL THEN     -- hasar tarihi
         v_dynamic_sql := v_dynamic_sql || ' AND EXISTS ( '
                                        || '    SELECT 1 FROM CLM_POL_BASES B1 '
                                        || '      WHERE A.CLAIM_ID = B1.CLAIM_ID '
                                        || '        AND B1.DATE_OF_LOSS BETWEEN TO_DATE(''' || p_claim_date_start ||''',''dd/MM/yyyy'')' || ' AND TO_DATE(''' || p_claim_date_end  || ''',''dd/MM/yyyy'')) ';
      END IF;


      IF (p_defendant_first_name IS NOT NULL OR p_defendant_surname IS NOT NULL) OR p_defendant_identity_no IS NOT NULL THEN
         v_dynamic_sql := v_dynamic_sql || ' AND EXISTS ( '
                                        || '    SELECT 1 FROM KOC_LAW_INTERESTED_PARTIES E1, CP_PARTNERS F1, KOC_CP_PARTNERS_EXT G1 '
                                        || '      WHERE E1.LAW_FILE_NO = H.LAW_FILE_NO '
                                        || '        AND E1.PART_ID = F1.PART_ID '
                                        || '        AND E1.PART_ID = G1.PART_ID (+) ';
         IF p_defendant_first_name IS NOT NULL OR p_defendant_surname IS NOT NULL THEN
            v_dynamic_sql := v_dynamic_sql
                                        || '        AND DECODE(F1.PARTNER_TYPE,''I'',F1.NAME,''P'',F1.FIRST_NAME||'' ''||F1.NAME) LIKE ''%' || NVL(p_defendant_first_name, '') || '%' || NVL(p_defendant_surname, '') || '%'' ';
         END IF;

         IF p_defendant_identity_no IS NOT NULL THEN
            v_dynamic_sql := v_dynamic_sql
                                        || '        AND (G1.IDENTITY_NO = ''' || p_defendant_identity_no || ''' OR G1.TAX_NUMBER = ''' || p_defendant_identity_no || ''') ';
         END IF;

         v_dynamic_sql := v_dynamic_sql
                                        || '        AND (E1.RES_DEF_TYPE = ''D'' OR E1.RES_DEF_TYPE = ''2'')) ';
      END IF;


      IF (p_plaintiff_first_name IS NOT NULL OR p_plaintiff_surname IS NOT NULL) OR p_plaintiff_identity_no IS NOT NULL THEN
         v_dynamic_sql := v_dynamic_sql || ' AND EXISTS ( '
                                        || '    SELECT 1 FROM KOC_LAW_INTERESTED_PARTIES E2, CP_PARTNERS F2, KOC_CP_PARTNERS_EXT G2 '
                                        || '      WHERE E2.LAW_FILE_NO = H.LAW_FILE_NO '
                                        || '        AND E2.PART_ID = F2.PART_ID '
                                        || '        AND E2.PART_ID = G2.PART_ID (+) ';
         IF p_plaintiff_first_name IS NOT NULL OR p_plaintiff_surname IS NOT NULL THEN
            v_dynamic_sql := v_dynamic_sql
                                        || '        AND DECODE(F2.PARTNER_TYPE,''I'',F2.NAME,''P'',F2.FIRST_NAME||'' ''||F2.NAME) LIKE ''%' || NVL(p_plaintiff_first_name, '') || '%' || NVL(p_plaintiff_surname, '') || '%'' ';
         END IF;

         IF p_plaintiff_identity_no IS NOT NULL THEN
            v_dynamic_sql := v_dynamic_sql
                                        || '        AND (G2.IDENTITY_NO = ''' || p_plaintiff_identity_no || ''' OR G2.TAX_NUMBER = ''' || p_plaintiff_identity_no || ''') ';
         END IF;

         v_dynamic_sql := v_dynamic_sql
                                        || '        AND (E2.RES_DEF_TYPE = ''R'' OR E2.RES_DEF_TYPE = ''1'')) ';
      END IF;

      IF (p_other_party_first_name IS NOT NULL OR p_other_party_surname IS NOT NULL) OR p_other_party_identity_no IS NOT NULL THEN

         v_dynamic_sql := v_dynamic_sql || ' AND EXISTS ( '
                                        || '    SELECT 1 FROM KOC_LAW_INTERESTED_PARTIES E3, CP_PARTNERS F3, KOC_CP_PARTNERS_EXT G3 '
                                        || '      WHERE E3.LAW_FILE_NO = H.LAW_FILE_NO '
                                        || '        AND E3.PART_ID = F3.PART_ID '
                                        || '        AND E3.PART_ID = G3.PART_ID (+) ';
         IF p_other_party_first_name IS NOT NULL OR p_other_party_surname IS NOT NULL THEN
            v_dynamic_sql := v_dynamic_sql
                                        || '        AND DECODE(F3.PARTNER_TYPE,''I'',F3.NAME,''P'',F3.FIRST_NAME||'' ''||F3.NAME) LIKE ''%' || NVL(p_other_party_first_name, '') || '%' || NVL(p_other_party_surname, '') || '%'' ';
         END IF;

         IF p_other_party_identity_no IS NOT NULL THEN
            v_dynamic_sql := v_dynamic_sql
                                        || '        AND (G3.IDENTITY_NO = ''' || p_other_party_identity_no || ''' OR G3.TAX_NUMBER = ''' || p_other_party_identity_no || ''') ';
         END IF;

         v_dynamic_sql := v_dynamic_sql
--                                        || '        AND (E3.RES_DEF_TYPE NOT IN (''R'',''D'', ''1'',''2'')) '
--                                        || '        AND E3.RES_DEF_TYPE <> (SELECT E4.RES_DEF_TYPE '
--                                        || '                                  FROM KOC_LAW_INTERESTED_PARTIES E4, '
--                                        || '                                       CP_PARTNERS F4 '
--                                        || '                                 WHERE E4.LAW_FILE_NO = H.LAW_FILE_NO '
--                                        || '                                   AND E4.PART_ID = F4.PART_ID '
--                                        || '                                   AND (E4.RES_DEF_TYPE IN (''R'',''D'') OR E4.RES_DEF_TYPE IN (''1'',''2'')) '
--                                        || '                                   AND (F4.NAME LIKE ''%ALLIANZ%'' OR F4.NAME LIKE ''%ALLÝANZ%'') AND ROWNUM = 1) ) ';
                                        || ') ';
      END IF;

      IF p_law_file_no IS NOT NULL THEN
         v_dynamic_sql := v_dynamic_sql || ' AND H.LAW_FILE_NO LIKE ''%' || p_law_file_no || ''' ';
      END IF;

      IF p_court_file_info IS NOT NULL OR (p_court_enf_no IS NOT NULL AND p_court_enf_type IS NOT NULL) THEN
         v_dynamic_sql := v_dynamic_sql || ' AND EXISTS ( '
                                        || '    SELECT 1 FROM KOC_LAW_BASE_COURTS E1 '
                                        || '      WHERE E1.LAW_FILE_NO = H.LAW_FILE_NO ';
         IF p_court_enf_no IS NOT NULL AND p_court_enf_type IS NOT NULL THEN
            v_dynamic_sql := v_dynamic_sql
                                        || '        AND E1.COURT_TYPE = ''' || p_court_enf_type || ''' AND E1.COURT_ID = ''' || p_court_enf_no || ''' ';
         END IF;

         IF p_court_file_info IS NOT NULL THEN
            v_dynamic_sql := v_dynamic_sql
                                        || '        AND E1.COURT_FILE_NO = ''' || p_court_file_info || ''' ';
         END IF;

         v_dynamic_sql := v_dynamic_sql
                                        || '        ) ';
      END IF;


      IF NVL(p_lawyer_part_id, 0) != 0 THEN    /* harici avukat */
         v_dynamic_sql := v_dynamic_sql || ' AND H.LAWYER = ''' || p_lawyer_part_id || ''' ';
      END IF;

      IF NVL(v_is_inlawyer, 0) = 0 THEN       -- 0 harici, >0 dahili demek harici avukatlarin sadece kendi dosyalarini görmelerini sagliyor
         v_dynamic_sql := v_dynamic_sql || ' AND H.LAWYER = (SELECT PARTNER_ID FROM WEB_V_SYSTEM_USERS WHERE  USER_NAME=''' || p_user_name || ''' AND ROWNUM = 1)';
      END IF;

      IF p_file_status IS NOT NULL THEN
         v_dynamic_sql := v_dynamic_sql || ' AND NVL(R.IS_CLOSED, 0) <> ''' || p_file_status || ''' ';
      END IF;

      IF p_file_open_date_start IS NOT NULL AND p_file_open_date_end IS NOT NULL THEN
         v_dynamic_sql := v_dynamic_sql || ' AND H.FILE_OPEN_DATE BETWEEN TO_DATE(''' || p_file_open_date_start ||''',''dd/MM/yyyy'')' || ' AND TO_DATE(''' || p_file_open_date_end  || ''',''dd/MM/yyyy'')';
      END IF;

      IF p_file_close_date_start IS NOT NULL AND p_file_close_date_end IS NOT NULL THEN
         v_dynamic_sql := v_dynamic_sql || ' AND H.FILE_CLOSE_DATE BETWEEN TO_DATE(''' || p_file_close_date_start || ''',''dd/MM/yyyy'')' || ' AND TO_DATE(''' || p_file_close_date_end || ''',''dd/MM/yyyy'')';
      END IF;

      IF p_court_date_start IS NOT NULL AND p_court_date_end IS NOT NULL THEN
         v_dynamic_sql := v_dynamic_sql || ' AND H.CASE_DATE BETWEEN TO_DATE(''' || p_court_date_start ||''',''dd/MM/yyyy'')' || ' AND TO_DATE(''' || p_court_date_end  || ''',''dd/MM/yyyy'')';
      END IF;

      IF p_opponent_identity_no IS NOT NULL THEN
         v_dynamic_sql := v_dynamic_sql || ' AND H.OPPOSITLAWYERTC =  ''' || p_opponent_identity_no || ''' ';
      END IF;

      IF p_in_lawyer IS NOT NULL THEN
         v_dynamic_sql := v_dynamic_sql || ' AND H.IN_LAWYER =  ''' || p_in_lawyer || ''' ';
      END IF;

      IF p_agency_id IS NOT NULL THEN
         v_dynamic_sql := v_dynamic_sql || ' AND H.AGENT_ROLE =  ''' || p_agency_id || ''' ';
      END IF;

      IF p_lawyer_reference IS NOT NULL THEN
         v_dynamic_sql := v_dynamic_sql || ' AND LOWER(H.LAWYER_REFERENCE) like ''%' || LOWER(p_lawyer_reference) || '%'' ';
      END IF;

      IF v_is_outlawyer > 0 THEN
        v_dynamic_sql := v_dynamic_sql || ' AND IS_APPROVED IN (0, 1, 2)';
      END IF;

--         v_dynamic_sql := v_dynamic_sql || ' ORDER BY H.LAW_FILE_NO ';

      v_dynamic_sql := v_dynamic_sql || ' ORDER BY H.LAW_FILE_NO, ';

      v_dynamic_sql := v_dynamic_sql || ' case
        when koc_clm_utils.partner_name_bul(E.part_id) like ''%ALL_ANZ%'' then
            1
        else
            2
      end
      ,
      case
        when H.law_case_type = ''A'' then
            case
                when decode(E.res_def_type, ''R'', ''1'', ''D'', ''2'', ''A'', ''3'', ''C'', ''7'', E.res_def_type) = ''2'' then
                    1
                else
                    2
            end
       else
            case
                when decode(E.res_def_type, ''R'', ''1'', ''D'', ''2'', ''A'', ''3'', ''C'', ''7'', E.res_def_type) = ''1'' then
                    1
                else
                    2
            end
      end';


      TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', 'GET_LAW_FILE', v_dynamic_sql);

      OPEN cur FOR v_dynamic_sql;


    EXCEPTION
    WHEN OTHERS THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
           'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
            'ALZ_LAW_TICKET_PROCESS_UTILS.GET_LAW_FILE',
            NULL,
            p_process_results);

    END;

PROCEDURE GET_LAWYER_LIST(cur                   OUT     refcur,
                          p_process_results     OUT     customer.process_result_table)
    IS
        v_result refcur;

    BEGIN
        OPEN v_result FOR
            SELECT C.PART_ID,
                CASE WHEN C.SURNAME IS NULL THEN C.NAME
                ELSE C.FIRST_NAME || ' ' || C.SURNAME
                END NAME
            FROM (
                SELECT DISTINCT B.PART_ID, B.FIRST_NAME, B.SURNAME, B.NAME
                    FROM KOC_LAW_BASES A, CP_PARTNERS B
                    WHERE A.LAWYER = B.PART_ID
            ) C
            ORDER BY NAME;
        cur := v_result;

    EXCEPTION
    WHEN OTHERS THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
           'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
            'ALZ_LAW_TICKET_PROCESS_UTILS.GET_LAWYER_LIST',
            NULL,
            p_process_results);
    END;

PROCEDURE GET_IN_LAWYER_LIST(cur                   OUT     refcur,
                             p_process_results     OUT     customer.process_result_table)
    IS
        v_result refcur;

    BEGIN
        OPEN v_result FOR
            SELECT DISTINCT ID, NAME_SURNAME, BRANCH, AZNET_USER
              FROM ALZ_LAWYERS
          ORDER BY NAME_SURNAME;
        cur := v_result;

    EXCEPTION
    WHEN OTHERS THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
           'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
            'ALZ_LAW_TICKET_PROCESS_UTILS.GET_IN_LAWYER_LIST',
            NULL,
            p_process_results);
    END;

PROCEDURE GET_OPPONENT_LAWYER_LIST(cur                  OUT     refcur,
                                   p_process_results    OUT     customer.process_result_table)
    IS
        v_result refcur;
    BEGIN
        OPEN v_result FOR
            SELECT DISTINCT OPPOSITLAWYERTC, OPPOSITLAWYERNAME
                FROM KOC_LAW_BASES
                WHERE OPPOSITLAWYERTC IS NOT NULL;
        cur := v_result;

    EXCEPTION
    WHEN OTHERS THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
           'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
            'ALZ_LAW_TICKET_PROCESS_UTILS.GET_OPPONENT_LAWYER_LIST',
            NULL,
            p_process_results);

    END;


PROCEDURE GET_FILE_FROM_UYAP(p_court_file_info           IN VARCHAR2,
                             p_defendant_identity_no     IN VARCHAR2,
                             p_defendant_first_name      IN VARCHAR2,
                             p_defendant_surname         IN VARCHAR2,
                             p_plaintiff_identity_no     IN VARCHAR2,
                             p_plaintiff_first_name      IN VARCHAR2,
                             p_plaintiff_surname         IN VARCHAR2,
                             p_other_party_identity_no   IN VARCHAR2,
                             p_other_party_first_name    IN VARCHAR2,
                             p_other_party_surname       IN VARCHAR2,
                             p_user_name                 IN VARCHAR2,
                             p_court_enf_no              IN VARCHAR2,
                             p_court_enf_type            IN VARCHAR2,
                             cur                         OUT refcur,
                             p_process_results           OUT     customer.process_result_table
                             )

      IS
         v_dynamic_sql       VARCHAR2(10000);

   BEGIN

      v_dynamic_sql:=      'SELECT '
                        || '      H.DOSYA_ID, '
                        || '      E.TARAF_ROL_KODU, '
                        || '      E.TARAF_ROLU, '
                        || '      DECODE(E.SOYAD,NULL,E.AD,E.AD||'' ''||E.SOYAD) NAME , '
                        || '      NVL(E.TC_KIMLIK_NO,E.VERGI_NO) IDENTITY_NO , '
                        || '      H.SIRKET_ADI, '
                        || '      H.DOSYA_NO , '
                        || '      H.BIRIM_ID COURT_ID, '
                        || '      ''U'' COURT_TYPE , '
                        || '      B.BIRIM_ADI '
                        || ' FROM '
                        || '      ALZ_UYAP_DOSYA_LISTESI H ,'
                        || '      ALZ_UYAP_TARAFLAR E, '
                        || '      ALZ_UYAP_BIRIM_LISTESI B '
                        || 'WHERE '
                        || '      H.DOSYA_ID = E.DOSYA_ID '
                        || '  AND H.SIRKET_ADI = E.SIRKET_ADI '
                        || '  AND H.BIRIM_ID = B.BIRIM_ID '
                        || '  AND H.GUNCEL_VERSIYON = ''Y'' '
                        || '  AND E.GUNCEL_VERSIYON = ''Y'' ';

      IF p_court_file_info IS NOT NULL THEN
         v_dynamic_sql := v_dynamic_sql || ' AND H.DOSYA_NO = ''' || p_court_file_info || ''' ' ;
      END IF;

      IF p_court_enf_no IS NOT NULL AND p_court_enf_type IS NOT NULL THEN
         v_dynamic_sql := v_dynamic_sql || ' AND H.BIRIM_ID = '''|| p_court_enf_no ||''' AND D.COURT_ENF_TYPE = ''' || p_court_enf_type ||''' ';
      END IF;

      IF (p_plaintiff_first_name IS NOT NULL AND p_plaintiff_surname IS NOT NULL) OR p_plaintiff_identity_no IS NOT NULL THEN
         v_dynamic_sql := v_dynamic_sql || ' AND EXISTS ( '
                                        || '    SELECT 1 FROM ALZ_UYAP_TARAFLAR E2 '
                                        || '      WHERE E2.DOSYA_ID = H.DOSYA_ID ';
         IF p_plaintiff_first_name IS NOT NULL AND p_plaintiff_surname IS NOT NULL THEN
            v_dynamic_sql := v_dynamic_sql
                                        || '        AND DECODE(E2.SOYAD,NULL,E2.AD,E2.AD||'' ''||E2.SOYAD) LIKE ''%' || p_plaintiff_first_name || '%' || p_plaintiff_surname || '%'' ';
         END IF;

         IF p_plaintiff_identity_no IS NOT NULL THEN
            v_dynamic_sql := v_dynamic_sql
                                        || '        AND (E2.TC_KIMLIK_NO = ''' || p_plaintiff_identity_no || ''' OR E2.VERGI_NO = ''' || p_plaintiff_identity_no || ''') ';
         END IF;

         v_dynamic_sql := v_dynamic_sql
                                        || '        AND E2.TARAF_ROL_KODU = 1) ';
      END IF;

      IF (p_defendant_first_name IS NOT NULL AND p_defendant_surname IS NOT NULL) OR p_defendant_identity_no IS NOT NULL THEN
         v_dynamic_sql := v_dynamic_sql || ' AND EXISTS ( '
                                        || '    SELECT 1 FROM ALZ_UYAP_TARAFLAR E2 '
                                        || '      WHERE E2.DOSYA_ID = H.DOSYA_ID ';
         IF p_defendant_first_name IS NOT NULL AND p_defendant_surname IS NOT NULL THEN
            v_dynamic_sql := v_dynamic_sql
                                        || '        AND DECODE(E2.SOYAD,NULL,E2.AD,E2.AD||'' ''||E2.SOYAD) LIKE ''%' || p_defendant_first_name || '%' || p_defendant_surname || '%'' ';
         END IF;

         IF p_defendant_identity_no IS NOT NULL THEN
            v_dynamic_sql := v_dynamic_sql
                                        || '        AND (E2.TC_KIMLIK_NO = ''' || p_defendant_identity_no || ''' OR E2.VERGI_NO = ''' || p_defendant_identity_no || ''') ';
         END IF;

         v_dynamic_sql := v_dynamic_sql
                                        || '        AND E2.TARAF_ROL_KODU = 2) ';
      END IF;

      IF (p_other_party_first_name IS NOT NULL AND p_other_party_surname IS NOT NULL) OR p_other_party_identity_no IS NOT NULL THEN
         v_dynamic_sql := v_dynamic_sql || ' AND EXISTS ( '
                                        || '    SELECT 1 FROM ALZ_UYAP_TARAFLAR E2 '
                                        || '      WHERE E2.DOSYA_ID = H.DOSYA_ID ';
         IF p_other_party_first_name IS NOT NULL AND p_other_party_surname IS NOT NULL THEN
            v_dynamic_sql := v_dynamic_sql
                                        || '        AND DECODE(E2.SOYAD,NULL,E2.AD,E2.AD||'' ''||E2.SOYAD) LIKE ''%' || p_other_party_first_name || '%' || p_other_party_surname || '%'' ';
         END IF;

         IF p_other_party_identity_no IS NOT NULL THEN
            v_dynamic_sql := v_dynamic_sql
                                        || '        AND (E2.TC_KIMLIK_NO = ''' || p_other_party_identity_no || ''' OR E2.VERGI_NO = ''' || p_other_party_identity_no || ''') ';
         END IF;

         v_dynamic_sql := v_dynamic_sql
                                        || '        AND E2.TARAF_ROL_KODU NOT IN (1, 2) ) ';
      END IF;

      v_dynamic_sql := v_dynamic_sql || ' ORDER BY H.DOSYA_ID ';

      TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', 'GET_FILE_FROM_UYAP', v_dynamic_sql);

      OPEN cur FOR v_dynamic_sql;


   EXCEPTION WHEN OTHERS THEN
    alz_web_process_utils.process_result (
        0,
        9,
        -1,
           'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
        'ALZ_LAW_TICKET_PROCESS_UTILS.GET_FILE_FROM_UYAP',
        NULL,
        p_process_results);


   END;


PROCEDURE GET_COURT_LIST(cur OUT refcur)
   IS
      v_result   refcur;
   BEGIN
      OPEN v_result FOR
         SELECT COURT_NAME,COURT_ID,COURT_TYPE
           FROM ALZ_LAW_COURTS_V
       ORDER BY COURT_NAME;

      cur := v_result;

   END;


PROCEDURE GET_POLICY_REF_AND_EXT_REF(p_law_file_no IN VARCHAR2,
                                     cur OUT refcur)
    IS
       v_result  refcur;
    BEGIN
       OPEN v_result FOR
          SELECT C.EXT_REFERENCE,B.POLICY_REF FROM
           Koc_law_bases_detail a,CLM_POL_BASES b,CLM_SUBFILES c WHERE
           A.CLAIM_ID = B.CLAIM_ID AND A.CLAIM_ID = C.CLAIM_ID AND A.LAW_FILE_NO = p_law_file_no;

       cur := v_result;

   END;


PROCEDURE GET_PARTY_ROLE(cur OUT refcur)
   IS
   BEGIN
      OPEN cur FOR
         SELECT TARAF_ROL_ACIKLAMA,TARAF_ROL_KODU
           FROM alz_uyap_taraf_rolleri WHERE TARAF_ROL_KODU NOT IN (1,2);
   END;


PROCEDURE GET_COMPANY_LIST_FROM_LOOK_UP(p_code IN VARCHAR2,
                                        p_cur  OUT refcur)
    IS
    BEGIN

     OPEN p_cur FOR
            SELECT PARAMETER , EXPLANATION
              FROM alz_look_up
             WHERE CODE = p_code;
    END;


PROCEDURE GET_DOCUMENT_TYPE_LIST(cur OUT refcur)
   IS
   BEGIN
      OPEN cur FOR
          SELECT   A.CODE, A.DOC_DESC, A.DOC_TYPE
            FROM   ALZ_LAW_DOC_TYPE_DEF A
        ORDER BY   CODE;
   END;


PROCEDURE GET_CLAIM_DOCUMENT_TYPE_LIST(cur OUT refcur)
   IS
   BEGIN
      OPEN cur FOR
          SELECT   -1 CODE, EXPLANATION DOC_DESC, 'Hasar Evraky' DOC_TYPE
            FROM   KOC_CLM_DOC_REF
        ORDER BY   EXPLANATION;
   END;


PROCEDURE GET_FILTER_DOCUMENT_TYPE_LIST(cur OUT refcur)
   IS
   BEGIN
      OPEN cur FOR
          SELECT   *
            FROM   (SELECT   A.CODE, A.DOC_DESC, A.DOC_TYPE
                      FROM   ALZ_LAW_DOC_TYPE_DEF A
                 UNION ALL
                    SELECT   -1 CODE, EXPLANATION DOC_DESC, 'Hasar Evraky' DOC_TYPE
                      FROM   KOC_CLM_DOC_REF)
        ORDER BY   DOC_DESC;
   END;

PROCEDURE GET_FILENET_MIMETYPE(p_uyap_mime_type     IN      VARCHAR2,
                               p_filenet_mime_type  OUT     VARCHAR2
                              )

    IS
        v_filenet_mime_type   VARCHAR2 (30) := NULL;

    BEGIN
        SELECT  FILENET_MIMETYPE
          INTO  v_filenet_mime_type
          FROM  ALZ_UYAP_FILENET_MIMETYPE_REF
         WHERE  UYAP_MIMETYPE = p_uyap_mime_type;

        p_filenet_mime_type := v_filenet_mime_type;

    EXCEPTION WHEN NO_DATA_FOUND THEN

        p_filenet_mime_type := NULL;

    END;

PROCEDURE GET_UYAP_MIMETYPE (p_filenet_mime_type      IN     VARCHAR2,
                             p_uyap_mime_type      OUT VARCHAR2)


    IS

        CURSOR c_uyap_mimetype(filenet_mime_type VARCHAR2)
        IS
        SELECT  UYAP_MIMETYPE
          FROM  ALZ_UYAP_FILENET_MIMETYPE_REF
         WHERE  LOWER(FILENET_MIMETYPE) = LOWER(filenet_mime_type);

    BEGIN

        OPEN c_uyap_mimetype(p_filenet_mime_type);
        FETCH c_uyap_mimetype INTO p_uyap_mime_type;
        CLOSE c_uyap_mimetype;

    END;

PROCEDURE GET_DAY_LEFT(p_cur                     OUT     refcur)

IS

BEGIN

     OPEN p_cur FOR
        SELECT TICKET_ID, OWNER, (TRUNC(DEADLINE_DATE) - TRUNC(SYSDATE)) AS DAY_LEFT FROM ALZ_LAW_TICKET_PROCESS
          WHERE (TRUNC(DEADLINE_DATE) - TRUNC(SYSDATE)) < 4
            AND STATUS NOT IN ('CLOSED','READ', 'CANCELLED')
            AND SEND_MAIL = 1;

END;

PROCEDURE GET_DAY_LEFT_FOR_MUHABERAT(p_cur                     OUT     refcur)

IS

BEGIN

     OPEN p_cur FOR
        SELECT TICKET_ID, OLD_OWNER, (TRUNC(MUHABERAT_DEADLINE_DATE) - TRUNC(SYSDATE)) AS DAY_LEFT, OWNER FROM ALZ_LAW_TICKET_PROCESS
          WHERE (TRUNC(MUHABERAT_DEADLINE_DATE) - TRUNC(SYSDATE)) < 0
            AND STATUS NOT IN ('CLOSED','READ', 'CANCELLED')
            AND SEND_MAIL = 1;

END;

PROCEDURE GET_EMAIL_FOR_UYAP_DOC (p_group        IN      VARCHAR2,
                                  cur           OUT       refcur)

IS
BEGIN

    OPEN cur FOR
        SELECT B.EMAIL,
               A.USER_NAME
          FROM WEB_SEC_SYSTEM_USERS A, CP_PARTNERS B
         WHERE A.CUSTOMER_PARTNER_ID = B.PART_ID AND
            EXISTS (select 1 from ALZ_LAW_GROUP_USER_REL C where C.GROUP_CODE = p_group AND A.USER_NAME = C.USERNAME  AND C.VALIDITY_START_DATE <= TRUNC(SYSDATE)
                          AND (C.VALIDITY_END_DATE IS NULL
                           OR C.VALIDITY_END_DATE >= TRUNC(SYSDATE)));

END;

PROCEDURE GET_NEW_DOC_TYPE_FROM_UYAP (cur OUT refcur)
IS
BEGIN

    OPEN cur FOR
        SELECT   DISTINCT A.TURU
          FROM   ALZ_UYAP_EVRAKLAR A
         WHERE   NOT EXISTS
                    (SELECT   1
                       FROM   ALZ_LAW_DOC_TYPE_DEF B
                      WHERE   REPLACE (LOWER (A.TURU), CHR (10), ' ') = LOWER (B.DOC_DESC)
                        AND   B.DOC_DESC IS NOT NULL)
           AND   A.TURU IS NOT NULL;

    ALZ_LAW_TICKET_PROCESS_UTILS.PROCEED_NEW_DOC_TYPE_FROM_UYAP;
END;

PROCEDURE PROCEED_NEW_DOC_TYPE_FROM_UYAP
IS

    CURSOR undefined_document_list
    IS
        SELECT   DISTINCT A.TURU
          FROM   ALZ_UYAP_EVRAKLAR A
         WHERE   NOT EXISTS
                    (SELECT   1
                       FROM   ALZ_LAW_DOC_TYPE_DEF B
                      WHERE   REPLACE (LOWER (A.TURU), CHR (10), ' ') = LOWER (B.DOC_DESC)
                        AND   B.DOC_DESC IS NOT NULL)
           AND   A.TURU IS NOT NULL;

BEGIN
    FOR doc_list IN undefined_document_list
    LOOP
        INSERT INTO ALZ_LAW_DOC_TYPE_DEF (CODE, SOURCE, DOC_TYPE, DOC_DESC)
             VALUES ((select (max(code) + 1) from alz_law_doc_type_def), 'UYAP', 'Uyap Evraky', doc_list.TURU);
    END LOOP doc_list;
END;

FUNCTION OWNER_BY_TICKETID(p_ticketId NUMBER) RETURN VARCHAR2
IS
   v_owner VARCHAR2(30) := NULL;
BEGIN

   select owner into v_owner from alz_law_ticket_process where ticket_Id = p_ticketId;

RETURN v_owner;

EXCEPTION WHEN NO_DATA_FOUND THEN
   RETURN NULL;

END;

FUNCTION CHECK_TAHKIM_FOR_LAST_DAY RETURN VARCHAR2
IS

   v_tahkim_ticket_for_last_day VARCHAR2(30) := NULL;

   CURSOR c_tahkim_ticket_for_last_day
   IS
      SELECT T.TICKET_ID
        FROM ALZ_LAW_TICKET T, ALZ_LAW_TICKET_PROCESS P
       WHERE T.SOURCE = 'TAHKIM' AND T.TICKET_ID = P.TICKET_ID AND P.CREATE_DATE > SYSDATE - 1;

   BEGIN

      OPEN c_tahkim_ticket_for_last_day;
      FETCH c_tahkim_ticket_for_last_day INTO v_tahkim_ticket_for_last_day;
      CLOSE c_tahkim_ticket_for_last_day;

      RETURN v_tahkim_ticket_for_last_day;
   END;

FUNCTION GET_DOC_DESCRIPTION(p_doc_code IN NUMBER, p_source IN VARCHAR2) RETURN VARCHAR2
IS

   v_doc_description VARCHAR2(100) := NULL;

   CURSOR c_doc_description(doc_code NUMBER, doc_source VARCHAR2)
   IS
      SELECT DOC_DESC
        FROM ALZ_LAW_DOC_TYPE_DEF
       WHERE CODE = doc_code AND SOURCE = doc_source;
   BEGIN

      OPEN c_doc_description(p_doc_code, p_source);
      FETCH c_doc_description INTO v_doc_description;
      CLOSE c_doc_description;

      RETURN v_doc_description;
   END;

PROCEDURE IS_FILENET_ID_TO_UPDATE(p_filenet_id VARCHAR2,  p_cur OUT refcur)
IS

   v_result refcur;

   BEGIN

      OPEN v_result FOR
         SELECT c.ticket_id, c.lawfile_no
         FROM koc_law_ext_documents_dtl b,
              alz_law_ticket c
         WHERE
              b.filenet_id = p_filenet_id and
              b.header_id = c.header_id;

      p_cur := v_result;
   END;

PROCEDURE GET_GROUP_LEVEL_USERS_INFO(p_group_code        IN         VARCHAR2,
                                     p_user_name         IN         VARCHAR2,
                                     cur                    OUT     refcur,
                                     p_process_results      OUT     customer.process_result_table)
    IS
        v_result refcur;
        v_user_group_code     VARCHAR2(10);
        v_dynamic_sql         VARCHAR2(20000);

        CURSOR c_user_group_name(p_user_name_ VARCHAR2)
            IS
        SELECT GROUP_CODE
          FROM ALZ_LAW_GROUP_USER_REL
         WHERE TRUNC(VALIDITY_START_DATE) <= TRUNC(SYSDATE)
           AND ( TRUNC(VALIDITY_END_DATE) >= SYSDATE OR VALIDITY_END_DATE IS NULL )
           AND USERNAME = p_user_name_;

    BEGIN
         OPEN c_user_group_name (p_user_name);
        FETCH c_user_group_name INTO v_user_group_code;
        CLOSE c_user_group_name;

        v_dynamic_sql := v_dynamic_sql || ' SELECT DISTINCT * FROM (SELECT koc_clm_utils.partner_name_bul(B.CUSTOMER_PARTNER_ID) NAME_SURNAME, A.GROUP_CODE, D.DEFINITION, A.USERNAME
                                              FROM ALZ_LAW_GROUP_USER_REL A, WEB_SEC_SYSTEM_USERS B, ALZ_LAWYERS C, ALZ_LAW_GROUP_DEF D,
                                                   (    SELECT LEVEL, LPAD ('''', (LEVEL)) || TO_CHAR (CODE) TREE
                                                          FROM ALZ_LAW_GROUP_DEF
                                                    START WITH PARENT IN (''' || p_group_code || ''')';
        v_dynamic_sql := v_dynamic_sql ||          'CONNECT BY PRIOR CODE = PARENT) B
                                                         WHERE ';

        IF p_user_name IS NULL THEN
            v_dynamic_sql := v_dynamic_sql || '    A.GROUP_CODE = ''' || p_group_code || ''' AND A.GROUP_CODE = B.TREE(+)';
        ELSIF p_user_name IS NOT NULL THEN
            v_dynamic_sql := v_dynamic_sql || '    B.TREE = A.GROUP_CODE';
        END IF;

        v_dynamic_sql := v_dynamic_sql || '    AND A.USERNAME = C.AZNET_USER(+)
                                               AND A.USERNAME = B.USER_NAME(+)
                                               AND A.GROUP_CODE = D.CODE
                                               AND TRUNC(A.VALIDITY_START_DATE) <= TRUNC(SYSDATE)
                                               AND ( TRUNC(A.VALIDITY_END_DATE) >= TRUNC(SYSDATE) OR A.VALIDITY_END_DATE IS NULL )
                                               AND TRUNC(D.VALIDITY_START_DATE) <= TRUNC(SYSDATE)
                                               AND ( TRUNC(D.VALIDITY_END_DATE) >= TRUNC(SYSDATE) OR D.VALIDITY_END_DATE IS NULL )
                                         UNION ALL
                                            SELECT koc_clm_utils.partner_name_bul(B.CUSTOMER_PARTNER_ID) NAME_SURNAME, A.GROUP_CODE, D.DEFINITION, A.USERNAME
                                              FROM ALZ_LAW_GROUP_USER_REL A, WEB_SEC_SYSTEM_USERS B, ALZ_LAWYERS C, ALZ_LAW_GROUP_DEF D
                                             WHERE A.USERNAME = C.AZNET_USER(+)
                                               AND A.USERNAME = B.USER_NAME(+)
                                               AND A.GROUP_CODE = D.CODE
                                               AND A.GROUP_CODE = ''' || p_group_code || '''
                                               AND A.USERNAME =  ''' || p_user_name || ''' --DECODE(v_user_group_code, ''LMMUNAUZ'',  p_user_name, DECODE(v_user_group_code, ''LMHA'',  p_user_name, DECODE(v_user_group_code, ''LMTPAD'',  p_user_name, DECODE(v_user_group_code, ''LMTPSE'',  p_user_name, A.USERNAME))))
                                               AND TRUNC(A.VALIDITY_START_DATE) <= TRUNC(SYSDATE)
                                               AND ( TRUNC(A.VALIDITY_END_DATE) >= TRUNC(SYSDATE) OR A.VALIDITY_END_DATE IS NULL )
                                               AND TRUNC(D.VALIDITY_START_DATE) <= TRUNC(SYSDATE)
                                               AND ( TRUNC(D.VALIDITY_END_DATE) >= TRUNC(SYSDATE) OR D.VALIDITY_END_DATE IS NULL ))
                                          ORDER BY NAME_SURNAME';

        TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', 'GET_GROUP_LEVEL_USERS_INFO', v_dynamic_sql);

        OPEN cur FOR v_dynamic_sql;

--         OPEN v_result FOR
--            SELECT DISTINCT * FROM (SELECT koc_clm_utils.partner_name_bul(B.CUSTOMER_PARTNER_ID) NAME_SURNAME, A.GROUP_CODE, D.DEFINITION, A.USERNAME
--              FROM ALZ_LAW_GROUP_USER_REL A, WEB_SEC_SYSTEM_USERS B, ALZ_LAWYERS C, ALZ_LAW_GROUP_DEF D,
--                   (    SELECT LEVEL, LPAD ('', (LEVEL)) || TO_CHAR (CODE) TREE
--                          FROM ALZ_LAW_GROUP_DEF
--                    START WITH PARENT IN (p_group_code)
--                    CONNECT BY PRIOR CODE = PARENT) B
--                         WHERE A.GROUP_CODE = p_group_code
--                           AND A.GROUP_CODE = B.TREE(+)
--                           AND A.USERNAME = C.AZNET_USER(+)
--                           AND A.USERNAME = B.USER_NAME(+)
--                           AND A.GROUP_CODE = D.CODE
--                           AND TRUNC(A.VALIDITY_START_DATE) <= TRUNC(SYSDATE)
--                           AND ( TRUNC(A.VALIDITY_END_DATE) >= TRUNC(SYSDATE) OR A.VALIDITY_END_DATE IS NULL )
--                           AND TRUNC(D.VALIDITY_START_DATE) <= TRUNC(SYSDATE)
--                           AND ( TRUNC(D.VALIDITY_END_DATE) >= TRUNC(SYSDATE) OR D.VALIDITY_END_DATE IS NULL )
--                     UNION ALL
--                        SELECT koc_clm_utils.partner_name_bul(B.CUSTOMER_PARTNER_ID) NAME_SURNAME, A.GROUP_CODE, D.DEFINITION, A.USERNAME
--                          FROM ALZ_LAW_GROUP_USER_REL A, WEB_SEC_SYSTEM_USERS B, ALZ_LAWYERS C, ALZ_LAW_GROUP_DEF D
--                         WHERE A.USERNAME = C.AZNET_USER(+)
--                           AND A.USERNAME = B.USER_NAME(+)
--                           AND A.GROUP_CODE = D.CODE
--                           AND A.GROUP_CODE = p_group_code
--                           AND A.USERNAME = p_user_name --DECODE(v_user_group_code, 'LMMUNAUZ',  p_user_name, DECODE(v_user_group_code, 'LMHA',  p_user_name, DECODE(v_user_group_code, 'LMTPAD',  p_user_name, DECODE(v_user_group_code, 'LMTPSE',  p_user_name, A.USERNAME))))
--                           AND TRUNC(A.VALIDITY_START_DATE) <= TRUNC(SYSDATE)
--                           AND ( TRUNC(A.VALIDITY_END_DATE) >= TRUNC(SYSDATE) OR A.VALIDITY_END_DATE IS NULL )
--                           AND TRUNC(D.VALIDITY_START_DATE) <= TRUNC(SYSDATE)
--                           AND ( TRUNC(D.VALIDITY_END_DATE) >= TRUNC(SYSDATE) OR D.VALIDITY_END_DATE IS NULL ))
--          ORDER BY NAME_SURNAME;
--        cur := v_result;

    EXCEPTION
    WHEN OTHERS THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
           'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
            'ALZ_LAW_TICKET_PROCESS_UTILS.GET_GROUP_USERS_INFO',
            NULL,
            p_process_results);
    END;

PROCEDURE GET_TICKET_RESPONSIBLE_LIST(p_user_name     IN             VARCHAR2,
                                      mainCur                OUT     refcur,
                                      childCur               OUT     refcur,
                                      p_process_results      OUT     customer.process_result_table)
    IS
        v_mainResult refcur;
        v_childResult refcur;
        v_user_group_code     VARCHAR2(10);
        v_dynamic_sql         VARCHAR2(20000);

        CURSOR c_user_group_name(p_user_name_ VARCHAR2)
            IS
        SELECT GROUP_CODE
          FROM ALZ_LAW_GROUP_USER_REL
         WHERE TRUNC(VALIDITY_START_DATE) <= TRUNC(SYSDATE)
           AND ( TRUNC(VALIDITY_END_DATE) >= SYSDATE OR VALIDITY_END_DATE IS NULL )
           AND USERNAME = p_user_name_;

    BEGIN
         OPEN c_user_group_name (p_user_name);
        FETCH c_user_group_name INTO v_user_group_code;
        CLOSE c_user_group_name;

        ALZ_LAW_TICKET_PROCESS_UTILS.GET_GROUP_LEVEL_USERS_INFO(v_user_group_code, p_user_name, mainCur, p_process_results);

        IF v_user_group_code <> 'LMHA' AND v_user_group_code <> 'LMMUNAUZ' AND v_user_group_code <> 'LMTPAD' AND v_user_group_code <> 'LMTPSE' THEN
            OPEN v_childResult FOR
               SELECT 'Harici Avukat' NAME_SURNAME, 'LMHA' GROUP_CODE, (select DEFINITION from alz_law_group_def where code = 'LMHA') DEFINITION, 'LMHA'  USERNAME FROM DUAL
            UNION ALL
               SELECT 'Muhaberat' NAME_SURNAME, 'LMMUNAUZ' GROUP_CODE, (select DEFINITION from alz_law_group_def where code = 'LMMUNAUZ') DEFINITION, 'LMMUNAUZ'  USERNAME FROM DUAL
            UNION ALL
               SELECT 'Adendum' NAME_SURNAME, 'LMTPAD' GROUP_CODE, (select DEFINITION from alz_law_group_def where code = 'LMTPAD') DEFINITION, 'LMTPAD'  USERNAME FROM DUAL
            UNION ALL
               SELECT 'Serebral' NAME_SURNAME, 'LMTPSE' GROUP_CODE, (select DEFINITION from alz_law_group_def where code = 'LMTPSE') DEFINITION, 'LMTPSE'  USERNAME FROM DUAL;
        END IF;

        childCur := v_childResult;

    EXCEPTION
    WHEN OTHERS THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
           'ORACLE_EXCEPTION',
            SUBSTR(SQLERRM || '--' || DBMS_UTILITY.format_error_backtrace(), 1, 1000),
            null,
            null,
            null,
            'ALZ_LAW_TICKET_PROCESS_UTILS.GET_LAW_TICKET_RESPONSIBLE_LIST',
            NULL,
            p_process_results);
    END;

    PROCEDURE GET_REASSIGNED_FILENETID_LIST(p_ticket_id       IN       NUMBER,
                                            p_source          IN       VARCHAR2,
                                            p_user_name       IN       VARCHAR2,
                                            cur                  OUT   refcur)

    IS

    BEGIN

        IF NVL(p_source, 'X') = 'UYAP' THEN
            OPEN cur FOR
                  SELECT A.TICKET_ID, A.UYAP_DOSYA_ID, A.UYAP_SAFAHAT_ID, A.UYAP_EVRAK_UNIQUE_ID, A.CODE, B.STATUS, B.OWNER, B.OWNER_GROUP, B.OLD_OWNER, B.OLD_OWNER_GROUP, UEI.FILENET_ID
                    FROM ALZ_LAW_TICKET A, ALZ_LAW_TICKET_PROCESS B, ALZ_UYAP_EVRAKLAR UE, ALZ_UYAP_EVRAK_ICERIK UEI
                   WHERE A.TICKET_ID = B.TICKET_ID
                     AND A.UYAP_DOSYA_ID = (select C.UYAP_DOSYA_ID from alz_law_ticket C where C.ticket_id = p_ticket_id)
                     AND A.SOURCE = 'UYAP'
                     AND B.TICKET_TYPE_ID = 2
                     AND A.UYAP_DOSYA_ID = UE.DOSYA_ID
                     AND A.UYAP_EVRAK_UNIQUE_ID = UE.UNIQUE_ID
                     AND UE.DOSYA_ID = UEI.DOSYA_ID
                     AND UE.EVRAK_ID = UEI.EVRAK_ID
                     AND UEI.FILENET_ID != 'noDocumentContentFound'
                     AND UEI.FILENET_ID IS NOT NULL;
        ELSIF NVL(p_source, 'X') = 'TAHKIM' THEN
            OPEN cur FOR
                  SELECT A.TICKET_ID, A.HEADER_ID, B.STATUS, B.OWNER, B.OWNER_GROUP, B.OLD_OWNER, B.OLD_OWNER_GROUP, D.FILENET_ID
                    FROM ALZ_LAW_TICKET A, ALZ_LAW_TICKET_PROCESS B, KOC_LAW_EXT_DOCUMENTS_DTL D
                   WHERE A.TICKET_ID = B.TICKET_ID
                     AND (A.COURT_FILE_NO = (select C.COURT_FILE_NO from alz_law_ticket C where C.ticket_id = p_ticket_id) or (a.court_file_no is null and a.ticket_id = p_ticket_id))
                     AND A.SOURCE = 'TAHKIM'
                     AND D.FILENET_ID != 'NO_DOCUMENT'
                     AND A.HEADER_ID = D.HEADER_ID
                     AND D.FILENET_ID IS NOT NULL;
        END IF;

    END;

    PROCEDURE REASSIGN_TICKET ( p_ticket_id         IN     NUMBER,
                                p_law_file_no       IN     VARCHAR2,
                                p_court_enf_type    IN     VARCHAR2,
                                p_court_no          IN     NUMBER,
                                p_court_file_no     IN     VARCHAR2,
                                p_operation         IN     VARCHAR2,
                                p_user_name         IN     VARCHAR2,
                                p_process_results      OUT customer.process_result_table )
    IS

        v_uyap_id               NUMBER;
        v_source                VARCHAR2(100);
        v_is_law_file_exists    NUMBER;

        v_source_type           VARCHAR2(100);
        v_param_id              VARCHAR2(100);
        v_ticket_type           NUMBER;
        v_bre_group             VARCHAR2(20);
        v_bre_user              VARCHAR2(20);
        v_bre_message           VARCHAR2(200);
        v_subject               VARCHAR2 (4000);
        v_bre_department        VARCHAR2(20);
        v_category              VARCHAR2(10);
        v_law_file_no           VARCHAR2(30);
        v_uyap_file_id          NUMBER;
        v_filenet_id            VARCHAR2(50);
        v_ticket_status         VARCHAR2(20);
        v_law_court_enf         VARCHAR2(10);
        v_court_file_no         VARCHAR2(50);
        v_uyap_birim_id         NUMBER;
        v_deadline              DATE;
        v_bre_term              NUMBER;
        v_rucu_no               VARCHAR2(200);
        v_company_name          VARCHAR2(20);
        v_send_mail             NUMBER;
        v_uyap_safahat_id       NUMBER;
        v_uyap_evrak_unique_id  NUMBER;
        v_header_id             NUMBER;
        v_bre_code              NUMBER;
        v_arrival_date          DATE;
        v_new_ticket_id         NUMBER;

        v_old_owner_grp         VARCHAR2(20);
        v_old_owner_usr         VARCHAR2(20);
        v_owner_grp             VARCHAR2(20);
        v_owner_usr             VARCHAR2(20);

        v_employee_id           VARCHAR2(30)    := NULL;
        v_employee_id_assigned  VARCHAR2(30)    := NULL;

        v_check_bre_param       NUMBER := 0;
        p_create_ticket_or_notif NUMBER := 1;
        v_new_subject        VARCHAR2 (4000);

        CURSOR c_related_ticket(v_ticket_id NUMBER)
            IS
        SELECT A.UYAP_DOSYA_ID, A.SOURCE, A.CODE
          FROM ALZ_LAW_TICKET A, ALZ_LAW_TICKET_PROCESS B
         WHERE A.TICKET_ID = B.TICKET_ID
           AND A.TICKET_ID = NVL(v_ticket_id, -1);

        CURSOR c_muhaberat_ticket(v_ticket_id NUMBER)
            IS
        SELECT A.TICKET_ID, A.FILENET_ID, B.STATUS, B.OWNER, B.OWNER_GROUP, B.OLD_OWNER, B.OLD_OWNER_GROUP
          FROM ALZ_LAW_TICKET A, ALZ_LAW_TICKET_PROCESS B
         WHERE A.TICKET_ID = B.TICKET_ID
           AND A.TICKET_ID = NVL(v_ticket_id, -1)
           AND SOURCE = 'MUHABERAT';

        CURSOR c_related_uyap_tickets(v_ticket_id NUMBER)
            IS
        SELECT A.TICKET_ID, A.UYAP_DOSYA_ID, A.UYAP_SAFAHAT_ID, A.UYAP_EVRAK_UNIQUE_ID, A.CODE, B.STATUS, B.OWNER, B.OWNER_GROUP, B.OLD_OWNER, B.OLD_OWNER_GROUP
          FROM ALZ_LAW_TICKET A, ALZ_LAW_TICKET_PROCESS B
         WHERE A.TICKET_ID = B.TICKET_ID
           AND A.UYAP_DOSYA_ID = (select C.UYAP_DOSYA_ID from alz_law_ticket C where C.ticket_id = v_ticket_id)
           AND A.SOURCE = 'UYAP'
           AND B.TICKET_TYPE_ID = 2;

        CURSOR c_related_tahkim_tickets(v_ticket_id NUMBER)
            IS
        SELECT A.TICKET_ID, A.HEADER_ID, B.STATUS, B.OWNER, B.OWNER_GROUP, B.OLD_OWNER, B.OLD_OWNER_GROUP
          FROM ALZ_LAW_TICKET A, ALZ_LAW_TICKET_PROCESS B
         WHERE A.TICKET_ID = B.TICKET_ID
           AND (A.COURT_FILE_NO = (select C.COURT_FILE_NO from alz_law_ticket C where C.ticket_id = v_ticket_id) or (a.court_file_no is null and a.ticket_id = p_ticket_id))
           AND ((NVL(p_operation, 'X') = 'UPDATE_DOC_TYPE' AND A.TICKET_ID = v_ticket_id)
                OR NVL(p_operation, 'X') <> 'UPDATE_DOC_TYPE')
           AND A.SOURCE = 'TAHKIM';


    BEGIN

         OPEN c_related_ticket (p_ticket_id);
        FETCH c_related_ticket INTO v_uyap_id, v_source, v_bre_code;
        CLOSE c_related_ticket;

        v_law_file_no := p_law_file_no;

        IF v_source = 'UYAP' THEN

            FOR related_uyap_ticket IN c_related_uyap_tickets(p_ticket_id) LOOP

                IF related_uyap_ticket.UYAP_EVRAK_UNIQUE_ID IS NULL AND related_uyap_ticket.UYAP_SAFAHAT_ID IS NULL THEN
                    v_source_type := 'UYAP_DOSYA';
                    v_param_id := related_uyap_ticket.UYAP_DOSYA_ID;
                ELSIF related_uyap_ticket.UYAP_EVRAK_UNIQUE_ID IS NOT NULL THEN
                    v_source_type := 'UYAP_EVRAK';
                    v_param_id := related_uyap_ticket.UYAP_EVRAK_UNIQUE_ID;
                ELSIF related_uyap_ticket.UYAP_SAFAHAT_ID IS NOT NULL THEN
                    v_source_type := 'UYAP_SAFAHAT';
                    v_param_id := related_uyap_ticket.UYAP_SAFAHAT_ID;
                END IF;

                --her bir ticket için out parametreleri nulllayýp mý ögndermek gerek?

                v_new_ticket_id := related_uyap_ticket.TICKET_ID;

                TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', 'zBEFORE-RA-CREATE_TICKET_PARAMETERS-GIRIS', v_source_type || ' - ' ||
                                                           null || ' - ' ||
                                                           v_param_id || ' - ' ||
                                                           null || ' - ' ||
                                                           null || ' - ' ||
                                                           p_user_name || ' - ' ||
                                                           null || ' - ' ||
                                                           null || ' - ' ||
                                                           v_new_ticket_id || ' - ' ||
                                                           v_ticket_type || ' - ' ||
                                                           v_bre_group || ' - ' ||
                                                           v_bre_user || ' - ' ||
                                                           v_bre_message || ' - ' ||
                                                           v_subject || ' - ' ||
                                                           v_bre_department || ' - ' ||
                                                           v_category || ' - ' ||
                                                           v_law_file_no || ' - ' ||
                                                           v_uyap_file_id || ' - ' ||
                                                           v_filenet_id || ' - ' ||
                                                           v_source || ' - ' ||
                                                           v_ticket_status || ' - ' ||
                                                           v_law_court_enf || ' - ' ||
                                                           v_court_file_no || ' - ' ||
                                                           v_uyap_birim_id || ' - ' ||
                                                           v_deadline || ' - ' ||
                                                           v_bre_term || ' - ' ||
                                                           v_rucu_no || ' - ' ||
                                                           v_company_name || ' - ' ||
                                                           v_send_mail || ' - ' ||
                                                           v_uyap_safahat_id || ' - ' ||
                                                           v_uyap_evrak_unique_id || ' - ' ||
                                                           v_header_id || ' - ' ||
                                                           v_bre_code || ' - ' ||
                                                           v_arrival_date || ' - ' ||
                                                           'PR' || ' - ' || '1' || ' - ' || v_check_bre_param || ' - ' || p_create_ticket_or_notif);

                ALZ_LAW_TICKET_PROCESS_UTILS.CREATE_TICKET_PARAMETERS(
                                                           v_source_type,
                                                           null,
                                                           v_param_id, --v_param_id, --dosya_id OR unique_evrak_id or unique_safahat_id
                                                           null,
                                                           null,
                                                           p_user_name,
                                                           null,
                                                           null,
                                                           v_new_ticket_id,
                                                           v_ticket_type,
                                                           v_bre_group,
                                                           v_bre_user,
                                                           v_bre_message,
                                                           v_subject,
                                                           v_bre_department,
                                                           v_category,
                                                           v_law_file_no,
                                                           v_uyap_file_id,
                                                           v_filenet_id,
                                                           v_source,
                                                           v_ticket_status,
                                                           v_law_court_enf,
                                                           v_court_file_no,
                                                           v_uyap_birim_id,
                                                           v_deadline,
                                                           v_bre_term,
                                                           v_rucu_no,
                                                           v_company_name,
                                                           v_send_mail,
                                                           v_uyap_safahat_id,
                                                           v_uyap_evrak_unique_id,
                                                           v_header_id,
                                                           v_bre_code,
                                                           v_arrival_date,
                                                           p_process_results, 1, v_check_bre_param, p_create_ticket_or_notif);

                v_law_file_no := p_law_file_no;

                TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', 'zAFTER-RA-CREATE_TICKET_PARAMETERS-CIKIS', v_source_type || ' - ' ||
                                                           null || ' - ' ||
                                                           v_param_id || ' - ' ||
                                                           null || ' - ' ||
                                                           null || ' - ' ||
                                                           p_user_name || ' - ' ||
                                                           null || ' - ' ||
                                                           null || ' - ' ||
                                                           v_new_ticket_id || ' - ' ||
                                                           v_ticket_type || ' - ' ||
                                                           v_bre_group || ' - ' ||
                                                           v_bre_user || ' - ' ||
                                                           v_bre_message || ' - ' ||
                                                           v_subject || ' - ' ||
                                                           v_bre_department || ' - ' ||
                                                           v_category || ' - ' ||
                                                           v_law_file_no || ' - ' ||
                                                           v_uyap_file_id || ' - ' ||
                                                           v_filenet_id || ' - ' ||
                                                           v_source || ' - ' ||
                                                           v_ticket_status || ' - ' ||
                                                           v_law_court_enf || ' - ' ||
                                                           v_court_file_no || ' - ' ||
                                                           v_uyap_birim_id || ' - ' ||
                                                           v_deadline || ' - ' ||
                                                           v_bre_term || ' - ' ||
                                                           v_rucu_no || ' - ' ||
                                                           v_company_name || ' - ' ||
                                                           v_send_mail || ' - ' ||
                                                           v_uyap_safahat_id || ' - ' ||
                                                           v_uyap_evrak_unique_id || ' - ' ||
                                                           v_header_id || ' - ' ||
                                                           v_bre_code || ' - ' ||
                                                           v_arrival_date || ' - ' ||
                                                           'PR' || ' - ' || '1' || ' - ' || v_check_bre_param || ' - ' || p_create_ticket_or_notif);

                IF p_process_results is not null and p_process_results.count > 0 THEN
                    for i in 1..(p_process_results.count) loop
                        begin
                            if p_process_results(i).type = -1 then
                                return;
                            end if;
                        end;
                    end loop;
                END IF;

                IF NVL(p_create_ticket_or_notif, 0) = 0 THEN
                    return;
                END IF;

                IF NVL(p_operation, 'X') = 'X' THEN
                    IF NOT (related_uyap_ticket.OWNER_GROUP = v_bre_group AND v_bre_group IN ('LMHDTOMUZ', 'LMKDTOMUZ')) AND
                       ((v_law_file_no is not null and koc_law_utils.check_law_file(v_law_file_no) > 0) OR nvl(v_law_file_no, 'NULLL') = 'NULLL')
                    THEN
                        v_new_ticket_id := related_uyap_ticket.TICKET_ID;

                        TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', 'zBEFORE-RA-CREATE_TICKET_PARAMETERS-GIRIS', v_source_type || ' - ' ||
                                                           null || ' - ' ||
                                                           v_param_id || ' - ' ||
                                                           null || ' - ' ||
                                                           null || ' - ' ||
                                                           p_user_name || ' - ' ||
                                                           null || ' - ' ||
                                                           null || ' - ' ||
                                                           v_new_ticket_id || ' - ' ||
                                                           v_ticket_type || ' - ' ||
                                                           v_bre_group || ' - ' ||
                                                           v_bre_user || ' - ' ||
                                                           v_bre_message || ' - ' ||
                                                           v_subject || ' - ' ||
                                                           v_bre_department || ' - ' ||
                                                           v_category || ' - ' ||
                                                           v_law_file_no || ' - ' ||
                                                           v_uyap_file_id || ' - ' ||
                                                           v_filenet_id || ' - ' ||
                                                           v_source || ' - ' ||
                                                           v_ticket_status || ' - ' ||
                                                           v_law_court_enf || ' - ' ||
                                                           v_court_file_no || ' - ' ||
                                                           v_uyap_birim_id || ' - ' ||
                                                           v_deadline || ' - ' ||
                                                           v_bre_term || ' - ' ||
                                                           v_rucu_no || ' - ' ||
                                                           v_company_name || ' - ' ||
                                                           v_send_mail || ' - ' ||
                                                           v_uyap_safahat_id || ' - ' ||
                                                           v_uyap_evrak_unique_id || ' - ' ||
                                                           v_header_id || ' - ' ||
                                                           v_bre_code || ' - ' ||
                                                           v_arrival_date || ' - ' ||
                                                           'PR' || ' - ' || '0' || ' - ' || v_check_bre_param || ' - ' || p_create_ticket_or_notif);

                        ALZ_LAW_TICKET_PROCESS_UTILS.CREATE_TICKET_PARAMETERS(
                                                               v_source_type,
                                                               null,
                                                               v_param_id, --v_param_id, --dosya_id OR unique_evrak_id or unique_safahat_id
                                                               null,
                                                               null,
                                                               p_user_name,
                                                               null,
                                                               null,
                                                               v_new_ticket_id,
                                                               v_ticket_type,
                                                               v_bre_group,
                                                               v_bre_user,
                                                               v_bre_message,
                                                               v_subject,
                                                               v_bre_department,
                                                               v_category,
                                                               v_law_file_no,
                                                               v_uyap_file_id,
                                                               v_filenet_id,
                                                               v_source,
                                                               v_ticket_status,
                                                               v_law_court_enf,
                                                               v_court_file_no,
                                                               v_uyap_birim_id,
                                                               v_deadline,
                                                               v_bre_term,
                                                               v_rucu_no,
                                                               v_company_name,
                                                               v_send_mail,
                                                               v_uyap_safahat_id,
                                                               v_uyap_evrak_unique_id,
                                                               v_header_id,
                                                               v_bre_code,
                                                               v_arrival_date,
                                                               p_process_results, 0, v_check_bre_param, p_create_ticket_or_notif);

                        v_law_file_no := p_law_file_no;

                        TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', 'zAFTER-RA-CREATE_TICKET_PARAMETERS-CIKIS', v_source_type || ' - ' ||
                                                           null || ' - ' ||
                                                           v_param_id || ' - ' ||
                                                           null || ' - ' ||
                                                           null || ' - ' ||
                                                           p_user_name || ' - ' ||
                                                           null || ' - ' ||
                                                           null || ' - ' ||
                                                           v_new_ticket_id || ' - ' ||
                                                           v_ticket_type || ' - ' ||
                                                           v_bre_group || ' - ' ||
                                                           v_bre_user || ' - ' ||
                                                           v_bre_message || ' - ' ||
                                                           v_subject || ' - ' ||
                                                           v_bre_department || ' - ' ||
                                                           v_category || ' - ' ||
                                                           v_law_file_no || ' - ' ||
                                                           v_uyap_file_id || ' - ' ||
                                                           v_filenet_id || ' - ' ||
                                                           v_source || ' - ' ||
                                                           v_ticket_status || ' - ' ||
                                                           v_law_court_enf || ' - ' ||
                                                           v_court_file_no || ' - ' ||
                                                           v_uyap_birim_id || ' - ' ||
                                                           v_deadline || ' - ' ||
                                                           v_bre_term || ' - ' ||
                                                           v_rucu_no || ' - ' ||
                                                           v_company_name || ' - ' ||
                                                           v_send_mail || ' - ' ||
                                                           v_uyap_safahat_id || ' - ' ||
                                                           v_uyap_evrak_unique_id || ' - ' ||
                                                           v_header_id || ' - ' ||
                                                           v_bre_code || ' - ' ||
                                                           v_arrival_date || ' - ' ||
                                                           'PR' || ' - ' || '0' || ' - ' || v_check_bre_param || ' - ' || p_create_ticket_or_notif);

                        IF p_process_results is not null and p_process_results.count > 0 THEN
                            for i in 1..(p_process_results.count) loop
                                begin
                                    if p_process_results(i).type = -1 then
                                        return;
                                    end if;
                                end;
                            end loop;
                        END IF;

                        IF NVL(p_create_ticket_or_notif, 0) = 0 THEN
                            return;
                        END IF;
                    ELSE
                        v_bre_user := related_uyap_ticket.OWNER;
                        v_bre_group := related_uyap_ticket.OWNER_GROUP;
                    END IF;
                ELSIF NVL(p_operation, 'X') = 'UPDATE_DOC_TYPE' THEN
                    v_bre_user := related_uyap_ticket.OWNER;
                END IF;


                IF related_uyap_ticket.STATUS IN ('OPEN', 'PAIRED', 'UNIFIED') THEN
                    v_old_owner_grp := related_uyap_ticket.OWNER_GROUP;
                    v_old_owner_usr := related_uyap_ticket.OWNER;
                    v_owner_grp := v_bre_group;
                    v_owner_usr := v_bre_user;
                ELSE
                    v_old_owner_grp := related_uyap_ticket.OLD_OWNER_GROUP;
                    v_old_owner_usr := related_uyap_ticket.OLD_OWNER;
                    v_owner_grp := related_uyap_ticket.OWNER_GROUP;
                    v_owner_usr := related_uyap_ticket.OWNER;
                END IF;

                UPDATE ALZ_LAW_TICKET
                   SET LAWFILE_NO = DECODE(p_law_file_no, 'NULLL', NULL, p_law_file_no),
                       SIRKET_ADI = v_company_name,
                       SUBJECT = DECODE(v_old_owner_usr, v_owner_usr, 'Hukuk dosya no bilgisi güncellendi.', 'Hukuk dosya no bilgisi güncelleme sonucu yeniden atama gerçekleþti.'),
                       UPDATE_USER = p_user_name,
                       UPDATE_DATE = SYSDATE
                 WHERE TICKET_ID = related_uyap_ticket.TICKET_ID;

                UPDATE ALZ_LAW_TICKET_PROCESS
                   SET DEADLINE_DATE = DECODE(NVL(v_bre_term, 0), 0, DEADLINE_DATE, (ARRIVAL_DATE + v_bre_term)),
                       DEPARTMENT = NVL(v_bre_department, DEPARTMENT),
                       OLD_OWNER = NVL(v_old_owner_usr, OLD_OWNER),
                       OLD_OWNER_GROUP = NVL(v_old_owner_grp, OLD_OWNER_GROUP),
                       OWNER = NVL(v_owner_usr, OWNER),
                       OWNER_GROUP = NVL(v_owner_grp, OWNER_GROUP),
                       UPDATE_USER = p_user_name,
                       UPDATE_DATE = SYSDATE,
                       REDIRECTOR = p_user_name,
                       RECEIVE_DATE = DECODE( v_owner_usr , related_uyap_ticket.OWNER , RECEIVE_DATE , SYSDATE  )
                 WHERE TICKET_ID = related_uyap_ticket.TICKET_ID;

                ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_TICKET_HISTORY(related_uyap_ticket.TICKET_ID, 'Hukuk dosya no bilgisi güncelleme sonucu yeniden atama gerçekleþti.', p_user_name);
            END LOOP related_ticket_list;

        ELSIF v_source = 'MUHABERAT' THEN

            v_source_type := 'MUHABERAT';
--            v_law_file_no := p_law_file_no;

            FOR muhaberat_ticket IN c_muhaberat_ticket(p_ticket_id) LOOP

                v_param_id := muhaberat_ticket.FILENET_ID;
                v_new_ticket_id := muhaberat_ticket.TICKET_ID;

                TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', 'zBEFORE-RA-CREATE_TICKET_PARAMETERS-GIRIS', v_source_type || ' - ' ||
                                                           null || ' - ' ||
                                                           v_param_id || ' - ' ||
                                                           null || ' - ' ||
                                                           null || ' - ' ||
                                                           p_user_name || ' - ' ||
                                                           null || ' - ' ||
                                                           null || ' - ' ||
                                                           v_new_ticket_id || ' - ' ||
                                                           v_ticket_type || ' - ' ||
                                                           v_bre_group || ' - ' ||
                                                           v_bre_user || ' - ' ||
                                                           v_bre_message || ' - ' ||
                                                           v_subject || ' - ' ||
                                                           v_bre_department || ' - ' ||
                                                           v_category || ' - ' ||
                                                           v_law_file_no || ' - ' ||
                                                           v_uyap_file_id || ' - ' ||
                                                           v_filenet_id || ' - ' ||
                                                           v_source || ' - ' ||
                                                           v_ticket_status || ' - ' ||
                                                           v_law_court_enf || ' - ' ||
                                                           v_court_file_no || ' - ' ||
                                                           v_uyap_birim_id || ' - ' ||
                                                           v_deadline || ' - ' ||
                                                           v_bre_term || ' - ' ||
                                                           v_rucu_no || ' - ' ||
                                                           v_company_name || ' - ' ||
                                                           v_send_mail || ' - ' ||
                                                           v_uyap_safahat_id || ' - ' ||
                                                           v_uyap_evrak_unique_id || ' - ' ||
                                                           v_header_id || ' - ' ||
                                                           v_bre_code || ' - ' ||
                                                           v_arrival_date || ' - ' ||
                                                           'PR' || ' - ' || '1' || ' - ' || v_check_bre_param || ' - ' || p_create_ticket_or_notif);

                ALZ_LAW_TICKET_PROCESS_UTILS.CREATE_TICKET_PARAMETERS(
                                                               v_source_type,
                                                               muhaberat_ticket.OWNER,
                                                               v_param_id, --v_param_id, --dosya_id OR unique_evrak_id or unique_safahat_id
                                                               null,
                                                               null,
                                                               p_user_name,
                                                               null,
                                                               null,
                                                               v_new_ticket_id,
                                                               v_ticket_type,
                                                               v_bre_group,
                                                               v_bre_user,
                                                               v_bre_message,
                                                               v_subject,
                                                               v_bre_department,
                                                               v_category,
                                                               v_law_file_no,
                                                               v_uyap_file_id,
                                                               v_filenet_id,
                                                               v_source,
                                                               v_ticket_status,
                                                               v_law_court_enf,
                                                               v_court_file_no,
                                                               v_uyap_birim_id,
                                                               v_deadline,
                                                               v_bre_term,
                                                               v_rucu_no,
                                                               v_company_name,
                                                               v_send_mail,
                                                               v_uyap_safahat_id,
                                                               v_uyap_evrak_unique_id,
                                                               v_header_id,
                                                               v_bre_code,
                                                               v_arrival_date,
                                                               p_process_results, 1, v_check_bre_param, p_create_ticket_or_notif);

                v_law_file_no := p_law_file_no;

                TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', 'zAFTER-RA-CREATE_TICKET_PARAMETERS-CIKIS', v_source_type || ' - ' ||
                                                           null || ' - ' ||
                                                           v_param_id || ' - ' ||
                                                           null || ' - ' ||
                                                           null || ' - ' ||
                                                           p_user_name || ' - ' ||
                                                           null || ' - ' ||
                                                           null || ' - ' ||
                                                           v_new_ticket_id || ' - ' ||
                                                           v_ticket_type || ' - ' ||
                                                           v_bre_group || ' - ' ||
                                                           v_bre_user || ' - ' ||
                                                           v_bre_message || ' - ' ||
                                                           v_subject || ' - ' ||
                                                           v_bre_department || ' - ' ||
                                                           v_category || ' - ' ||
                                                           v_law_file_no || ' - ' ||
                                                           v_uyap_file_id || ' - ' ||
                                                           v_filenet_id || ' - ' ||
                                                           v_source || ' - ' ||
                                                           v_ticket_status || ' - ' ||
                                                           v_law_court_enf || ' - ' ||
                                                           v_court_file_no || ' - ' ||
                                                           v_uyap_birim_id || ' - ' ||
                                                           v_deadline || ' - ' ||
                                                           v_bre_term || ' - ' ||
                                                           v_rucu_no || ' - ' ||
                                                           v_company_name || ' - ' ||
                                                           v_send_mail || ' - ' ||
                                                           v_uyap_safahat_id || ' - ' ||
                                                           v_uyap_evrak_unique_id || ' - ' ||
                                                           v_header_id || ' - ' ||
                                                           v_bre_code || ' - ' ||
                                                           v_arrival_date || ' - ' ||
                                                           'PR' || ' - ' || '1' || ' - ' || v_check_bre_param || ' - ' || p_create_ticket_or_notif);

                IF p_process_results is not null and p_process_results.count > 0 THEN
                    for i in 1..(p_process_results.count) loop
                        begin
                            if p_process_results(i).type = -1 then
                                return;
                            end if;
                        end;
                    end loop;
                END IF;

                IF NVL(p_create_ticket_or_notif, 0) = 0 THEN
                    return;
                END IF;

                IF ((v_law_file_no is not null and koc_law_utils.check_law_file(v_law_file_no) > 0) OR nvl(v_law_file_no, 'NULLL') = 'NULLL')
                   AND
                   ((NVL(p_operation, 'X') = 'X' AND NOT (muhaberat_ticket.OWNER_GROUP = v_bre_group AND v_bre_group IN ('LMHDTOMUZ', 'LMKDTOMUZ')))
                    OR
                   (NVL(p_operation, 'X') = 'UPDATE_DOC_TYPE' AND muhaberat_ticket.OWNER_GROUP <> v_bre_group))
                THEN
                    v_new_ticket_id := muhaberat_ticket.TICKET_ID;

                    TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', 'zBEFORE-RA-CREATE_TICKET_PARAMETERS-GIRIS', v_source_type || ' - ' ||
                                                       null || ' - ' ||
                                                       v_param_id || ' - ' ||
                                                       null || ' - ' ||
                                                       null || ' - ' ||
                                                       p_user_name || ' - ' ||
                                                       null || ' - ' ||
                                                       null || ' - ' ||
                                                       v_new_ticket_id || ' - ' ||
                                                       v_ticket_type || ' - ' ||
                                                       v_bre_group || ' - ' ||
                                                       v_bre_user || ' - ' ||
                                                       v_bre_message || ' - ' ||
                                                       v_subject || ' - ' ||
                                                       v_bre_department || ' - ' ||
                                                       v_category || ' - ' ||
                                                       v_law_file_no || ' - ' ||
                                                       v_uyap_file_id || ' - ' ||
                                                       v_filenet_id || ' - ' ||
                                                       v_source || ' - ' ||
                                                       v_ticket_status || ' - ' ||
                                                       v_law_court_enf || ' - ' ||
                                                       v_court_file_no || ' - ' ||
                                                       v_uyap_birim_id || ' - ' ||
                                                       v_deadline || ' - ' ||
                                                       v_bre_term || ' - ' ||
                                                       v_rucu_no || ' - ' ||
                                                       v_company_name || ' - ' ||
                                                       v_send_mail || ' - ' ||
                                                       v_uyap_safahat_id || ' - ' ||
                                                       v_uyap_evrak_unique_id || ' - ' ||
                                                       v_header_id || ' - ' ||
                                                       v_bre_code || ' - ' ||
                                                       v_arrival_date || ' - ' ||
                                                       'PR' || ' - ' || '0' || ' - ' || v_check_bre_param || ' - ' || p_create_ticket_or_notif);

                    ALZ_LAW_TICKET_PROCESS_UTILS.CREATE_TICKET_PARAMETERS(
                                                               v_source_type,
                                                               muhaberat_ticket.OWNER,
                                                               v_param_id, --v_param_id, --dosya_id OR unique_evrak_id or unique_safahat_id
                                                               null,
                                                               null,
                                                               p_user_name,
                                                               null,
                                                               null,
                                                               v_new_ticket_id,
                                                               v_ticket_type,
                                                               v_bre_group,
                                                               v_bre_user,
                                                               v_bre_message,
                                                               v_subject,
                                                               v_bre_department,
                                                               v_category,
                                                               v_law_file_no,
                                                               v_uyap_file_id,
                                                               v_filenet_id,
                                                               v_source,
                                                               v_ticket_status,
                                                               v_law_court_enf,
                                                               v_court_file_no,
                                                               v_uyap_birim_id,
                                                               v_deadline,
                                                               v_bre_term,
                                                               v_rucu_no,
                                                               v_company_name,
                                                               v_send_mail,
                                                               v_uyap_safahat_id,
                                                               v_uyap_evrak_unique_id,
                                                               v_header_id,
                                                               v_bre_code,
                                                               v_arrival_date,
                                                               p_process_results, 0, v_check_bre_param, p_create_ticket_or_notif);

                    v_law_file_no := p_law_file_no;

                    TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', 'zAFTER-RA-CREATE_TICKET_PARAMETERS-CIKIS', v_source_type || ' - ' ||
                                                       null || ' - ' ||
                                                       v_param_id || ' - ' ||
                                                       null || ' - ' ||
                                                       null || ' - ' ||
                                                       p_user_name || ' - ' ||
                                                       null || ' - ' ||
                                                       null || ' - ' ||
                                                       v_new_ticket_id || ' - ' ||
                                                       v_ticket_type || ' - ' ||
                                                       v_bre_group || ' - ' ||
                                                       v_bre_user || ' - ' ||
                                                       v_bre_message || ' - ' ||
                                                       v_subject || ' - ' ||
                                                       v_bre_department || ' - ' ||
                                                       v_category || ' - ' ||
                                                       v_law_file_no || ' - ' ||
                                                       v_uyap_file_id || ' - ' ||
                                                       v_filenet_id || ' - ' ||
                                                       v_source || ' - ' ||
                                                       v_ticket_status || ' - ' ||
                                                       v_law_court_enf || ' - ' ||
                                                       v_court_file_no || ' - ' ||
                                                       v_uyap_birim_id || ' - ' ||
                                                       v_deadline || ' - ' ||
                                                       v_bre_term || ' - ' ||
                                                       v_rucu_no || ' - ' ||
                                                       v_company_name || ' - ' ||
                                                       v_send_mail || ' - ' ||
                                                       v_uyap_safahat_id || ' - ' ||
                                                       v_uyap_evrak_unique_id || ' - ' ||
                                                       v_header_id || ' - ' ||
                                                       v_bre_code || ' - ' ||
                                                       v_arrival_date || ' - ' ||
                                                       'PR' || ' - ' || '0' || ' - ' || v_check_bre_param || ' - ' || p_create_ticket_or_notif);

                    IF p_process_results is not null and p_process_results.count > 0 THEN
                        for i in 1..(p_process_results.count) loop
                            begin
                                if p_process_results(i).type = -1 then
                                    return;
                                end if;
                            end;
                        end loop;
                    END IF;

                    IF NVL(p_create_ticket_or_notif, 0) = 0 THEN
                        return;
                    END IF;
                ELSE
                    v_bre_user := muhaberat_ticket.OWNER;
                    v_bre_group := muhaberat_ticket.OWNER_GROUP;
                END IF;


                IF muhaberat_ticket.STATUS IN ('OPEN', 'PAIRED', 'UNIFIED') THEN
                    v_old_owner_grp := muhaberat_ticket.OWNER_GROUP;
                    v_old_owner_usr := muhaberat_ticket.OWNER;
                    v_owner_grp := v_bre_group;
                    v_owner_usr := v_bre_user;
                ELSE
                    v_old_owner_grp := muhaberat_ticket.OLD_OWNER_GROUP;
                    v_old_owner_usr := muhaberat_ticket.OLD_OWNER;
                    v_owner_grp := muhaberat_ticket.OWNER_GROUP;
                    v_owner_usr := muhaberat_ticket.OWNER;
                END IF;

                if nvl(p_operation, 'X') = 'UPDATE_DOC_TYPE' then
                    v_new_subject := 'Evrak türü güncelleme sonucu yeniden atama gerçekleþti.';

                elsif v_old_owner_usr = v_owner_usr then
                    v_new_subject := 'Hukuk dosya no bilgisi güncellendi.';

                else
                    v_new_subject := 'Hukuk dosya no bilgisi güncelleme sonucu yeniden atama gerçekleþti.';
                end if;

                UPDATE ALZ_LAW_TICKET
                   SET LAWFILE_NO = DECODE(p_law_file_no, 'NULLL', NULL, p_law_file_no),
                       COURT_FILE_NO = p_court_file_no,
                       LAW_COURT_ENF = p_court_enf_type,
                       UYAP_BIRIM_ID = p_court_no,
                       SIRKET_ADI = v_company_name,
                       SUBJECT = v_new_subject,
                       UPDATE_USER = p_user_name,
                       UPDATE_DATE = SYSDATE
                 WHERE TICKET_ID = muhaberat_ticket.TICKET_ID;

                UPDATE ALZ_LAW_TICKET_PROCESS
                   SET DEADLINE_DATE = DECODE(NVL(v_bre_term, 0), 0, DEADLINE_DATE, (ARRIVAL_DATE + v_bre_term)),
                       DEPARTMENT = NVL(v_bre_department, DEPARTMENT),
                       OLD_OWNER = NVL(v_old_owner_usr, OLD_OWNER),
                       OLD_OWNER_GROUP = NVL(v_old_owner_grp, OLD_OWNER_GROUP),
                       OWNER = NVL(v_owner_usr, OWNER),
                       OWNER_GROUP = NVL(v_owner_grp, OWNER_GROUP),
                       UPDATE_USER = p_user_name,
                       UPDATE_DATE = SYSDATE,
                       REDIRECTOR = p_user_name,
                       RECEIVE_DATE = DECODE( v_owner_usr , muhaberat_ticket.OWNER , RECEIVE_DATE , SYSDATE  )
                 WHERE TICKET_ID = muhaberat_ticket.TICKET_ID;

                ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_TICKET_HISTORY(muhaberat_ticket.TICKET_ID, v_new_subject, p_user_name);

            END LOOP;
        ELSIF v_source = 'TAHKIM' THEN

            v_source_type := 'TAHKIM';

            FOR related_tahkim_ticket IN c_related_tahkim_tickets(p_ticket_id) LOOP

                v_param_id := related_tahkim_ticket.HEADER_ID;
                v_new_ticket_id := related_tahkim_ticket.TICKET_ID;
--                v_law_file_no := p_law_file_no;

                TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', 'zBEFORE-RA-CREATE_TICKET_PARAMETERS-GIRIS', v_source_type || ' - ' ||
                                                           null || ' - ' ||
                                                           v_param_id || ' - ' ||
                                                           null || ' - ' ||
                                                           null || ' - ' ||
                                                           p_user_name || ' - ' ||
                                                           null || ' - ' ||
                                                           null || ' - ' ||
                                                           v_new_ticket_id || ' - ' ||
                                                           v_ticket_type || ' - ' ||
                                                           v_bre_group || ' - ' ||
                                                           v_bre_user || ' - ' ||
                                                           v_bre_message || ' - ' ||
                                                           v_subject || ' - ' ||
                                                           v_bre_department || ' - ' ||
                                                           v_category || ' - ' ||
                                                           v_law_file_no || ' - ' ||
                                                           v_uyap_file_id || ' - ' ||
                                                           v_filenet_id || ' - ' ||
                                                           v_source || ' - ' ||
                                                           v_ticket_status || ' - ' ||
                                                           v_law_court_enf || ' - ' ||
                                                           v_court_file_no || ' - ' ||
                                                           v_uyap_birim_id || ' - ' ||
                                                           v_deadline || ' - ' ||
                                                           v_bre_term || ' - ' ||
                                                           v_rucu_no || ' - ' ||
                                                           v_company_name || ' - ' ||
                                                           v_send_mail || ' - ' ||
                                                           v_uyap_safahat_id || ' - ' ||
                                                           v_uyap_evrak_unique_id || ' - ' ||
                                                           v_header_id || ' - ' ||
                                                           v_bre_code || ' - ' ||
                                                           v_arrival_date || ' - ' ||
                                                           'PR' || ' - ' || '1' || ' - ' || v_check_bre_param || ' - ' || p_create_ticket_or_notif);

                ALZ_LAW_TICKET_PROCESS_UTILS.CREATE_TICKET_PARAMETERS(
                                                           v_source_type,
                                                           null,
                                                           v_param_id, --v_param_id, --dosya_id OR unique_evrak_id or unique_safahat_id
                                                           null,
                                                           null,
                                                           p_user_name,
                                                           null,
                                                           null,
                                                           v_new_ticket_id,
                                                           v_ticket_type,
                                                           v_bre_group,
                                                           v_bre_user,
                                                           v_bre_message,
                                                           v_subject,
                                                           v_bre_department,
                                                           v_category,
                                                           v_law_file_no,
                                                           v_uyap_file_id,
                                                           v_filenet_id,
                                                           v_source,
                                                           v_ticket_status,
                                                           v_law_court_enf,
                                                           v_court_file_no,
                                                           v_uyap_birim_id,
                                                           v_deadline,
                                                           v_bre_term,
                                                           v_rucu_no,
                                                           v_company_name,
                                                           v_send_mail,
                                                           v_uyap_safahat_id,
                                                           v_uyap_evrak_unique_id,
                                                           v_header_id,
                                                           v_bre_code,
                                                           v_arrival_date,
                                                           p_process_results, 1, v_check_bre_param, p_create_ticket_or_notif);

                v_law_file_no := p_law_file_no;

                TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', 'zAFTER-RA-CREATE_TICKET_PARAMETERS-CIKIS', v_source_type || ' - ' ||
                                                           null || ' - ' ||
                                                           v_param_id || ' - ' ||
                                                           null || ' - ' ||
                                                           null || ' - ' ||
                                                           p_user_name || ' - ' ||
                                                           null || ' - ' ||
                                                           null || ' - ' ||
                                                           v_new_ticket_id || ' - ' ||
                                                           v_ticket_type || ' - ' ||
                                                           v_bre_group || ' - ' ||
                                                           v_bre_user || ' - ' ||
                                                           v_bre_message || ' - ' ||
                                                           v_subject || ' - ' ||
                                                           v_bre_department || ' - ' ||
                                                           v_category || ' - ' ||
                                                           v_law_file_no || ' - ' ||
                                                           v_uyap_file_id || ' - ' ||
                                                           v_filenet_id || ' - ' ||
                                                           v_source || ' - ' ||
                                                           v_ticket_status || ' - ' ||
                                                           v_law_court_enf || ' - ' ||
                                                           v_court_file_no || ' - ' ||
                                                           v_uyap_birim_id || ' - ' ||
                                                           v_deadline || ' - ' ||
                                                           v_bre_term || ' - ' ||
                                                           v_rucu_no || ' - ' ||
                                                           v_company_name || ' - ' ||
                                                           v_send_mail || ' - ' ||
                                                           v_uyap_safahat_id || ' - ' ||
                                                           v_uyap_evrak_unique_id || ' - ' ||
                                                           v_header_id || ' - ' ||
                                                           v_bre_code || ' - ' ||
                                                           v_arrival_date || ' - ' ||
                                                           'PR' || ' - ' || '1' || ' - ' || v_check_bre_param || ' - ' || p_create_ticket_or_notif);

                IF p_process_results is not null and p_process_results.count > 0 THEN
                    for i in 1..(p_process_results.count) loop
                        begin
                            if p_process_results(i).type = -1 then
                                return;
                            end if;
                        end;
                    end loop;
                END IF;

                IF NVL(p_create_ticket_or_notif, 0) = 0 THEN
                    return;
                END IF;

                    IF ((v_law_file_no is not null and koc_law_utils.check_law_file(v_law_file_no) > 0) OR nvl(v_law_file_no, 'NULLL') = 'NULLL')
                       AND
                       ((NVL(p_operation, 'X') = 'X' AND NOT (related_tahkim_ticket.OWNER_GROUP = v_bre_group AND v_bre_group IN ('LMHDTOMUZ', 'LMKDTOMUZ')))
                       OR
                       (NVL(p_operation, 'X') = 'UPDATE_DOC_TYPE' AND related_tahkim_ticket.OWNER_GROUP <> v_bre_group))
                    THEN
                        v_new_ticket_id := related_tahkim_ticket.TICKET_ID;

                        TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', 'zBEFORE-RA-CREATE_TICKET_PARAMETERS-GIRIS', v_source_type || ' - ' ||
                                                           null || ' - ' ||
                                                           v_param_id || ' - ' ||
                                                           null || ' - ' ||
                                                           null || ' - ' ||
                                                           p_user_name || ' - ' ||
                                                           null || ' - ' ||
                                                           null || ' - ' ||
                                                           v_new_ticket_id || ' - ' ||
                                                           v_ticket_type || ' - ' ||
                                                           v_bre_group || ' - ' ||
                                                           v_bre_user || ' - ' ||
                                                           v_bre_message || ' - ' ||
                                                           v_subject || ' - ' ||
                                                           v_bre_department || ' - ' ||
                                                           v_category || ' - ' ||
                                                           v_law_file_no || ' - ' ||
                                                           v_uyap_file_id || ' - ' ||
                                                           v_filenet_id || ' - ' ||
                                                           v_source || ' - ' ||
                                                           v_ticket_status || ' - ' ||
                                                           v_law_court_enf || ' - ' ||
                                                           v_court_file_no || ' - ' ||
                                                           v_uyap_birim_id || ' - ' ||
                                                           v_deadline || ' - ' ||
                                                           v_bre_term || ' - ' ||
                                                           v_rucu_no || ' - ' ||
                                                           v_company_name || ' - ' ||
                                                           v_send_mail || ' - ' ||
                                                           v_uyap_safahat_id || ' - ' ||
                                                           v_uyap_evrak_unique_id || ' - ' ||
                                                           v_header_id || ' - ' ||
                                                           v_bre_code || ' - ' ||
                                                           v_arrival_date || ' - ' ||
                                                           'PR' || ' - ' || '0' || ' - ' || v_check_bre_param || ' - ' || p_create_ticket_or_notif);

                        ALZ_LAW_TICKET_PROCESS_UTILS.CREATE_TICKET_PARAMETERS(
                                                               v_source_type,
                                                               null,
                                                               v_param_id, --v_param_id, --dosya_id OR unique_evrak_id or unique_safahat_id
                                                               null,
                                                               null,
                                                               p_user_name,
                                                               null,
                                                               null,
                                                               v_new_ticket_id,
                                                               v_ticket_type,
                                                               v_bre_group,
                                                               v_bre_user,
                                                               v_bre_message,
                                                               v_subject,
                                                               v_bre_department,
                                                               v_category,
                                                               v_law_file_no,
                                                               v_uyap_file_id,
                                                               v_filenet_id,
                                                               v_source,
                                                               v_ticket_status,
                                                               v_law_court_enf,
                                                               v_court_file_no,
                                                               v_uyap_birim_id,
                                                               v_deadline,
                                                               v_bre_term,
                                                               v_rucu_no,
                                                               v_company_name,
                                                               v_send_mail,
                                                               v_uyap_safahat_id,
                                                               v_uyap_evrak_unique_id,
                                                               v_header_id,
                                                               v_bre_code,
                                                               v_arrival_date,
                                                               p_process_results, 0, v_check_bre_param, p_create_ticket_or_notif);

                        v_law_file_no := p_law_file_no;

                        TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', 'zAFTER-RA-CREATE_TICKET_PARAMETERS-CIKIS', v_source_type || ' - ' ||
                                                           null || ' - ' ||
                                                           v_param_id || ' - ' ||
                                                           null || ' - ' ||
                                                           null || ' - ' ||
                                                           p_user_name || ' - ' ||
                                                           null || ' - ' ||
                                                           null || ' - ' ||
                                                           v_new_ticket_id || ' - ' ||
                                                           v_ticket_type || ' - ' ||
                                                           v_bre_group || ' - ' ||
                                                           v_bre_user || ' - ' ||
                                                           v_bre_message || ' - ' ||
                                                           v_subject || ' - ' ||
                                                           v_bre_department || ' - ' ||
                                                           v_category || ' - ' ||
                                                           v_law_file_no || ' - ' ||
                                                           v_uyap_file_id || ' - ' ||
                                                           v_filenet_id || ' - ' ||
                                                           v_source || ' - ' ||
                                                           v_ticket_status || ' - ' ||
                                                           v_law_court_enf || ' - ' ||
                                                           v_court_file_no || ' - ' ||
                                                           v_uyap_birim_id || ' - ' ||
                                                           v_deadline || ' - ' ||
                                                           v_bre_term || ' - ' ||
                                                           v_rucu_no || ' - ' ||
                                                           v_company_name || ' - ' ||
                                                           v_send_mail || ' - ' ||
                                                           v_uyap_safahat_id || ' - ' ||
                                                           v_uyap_evrak_unique_id || ' - ' ||
                                                           v_header_id || ' - ' ||
                                                           v_bre_code || ' - ' ||
                                                           v_arrival_date || ' - ' ||
                                                           'PR' || ' - ' || '0' || ' - ' || v_check_bre_param || ' - ' || p_create_ticket_or_notif);

                        IF p_process_results is not null and p_process_results.count > 0 THEN
                            for i in 1..(p_process_results.count) loop
                                begin
                                    if p_process_results(i).type = -1 then
                                        return;
                                    end if;
                                end;
                            end loop;
                        END IF;

                        IF NVL(p_create_ticket_or_notif, 0) = 0 THEN
                            return;
                        END IF;
                    ELSE
                        v_bre_user := related_tahkim_ticket.OWNER;
                        v_bre_group := related_tahkim_ticket.OWNER_GROUP;
                    END IF;

                IF related_tahkim_ticket.STATUS IN ('OPEN', 'PAIRED', 'UNIFIED') THEN
                    v_old_owner_grp := related_tahkim_ticket.OWNER_GROUP;
                    v_old_owner_usr := related_tahkim_ticket.OWNER;
                    v_owner_grp := v_bre_group;
                    v_owner_usr := v_bre_user;
                ELSE
                    v_old_owner_grp := related_tahkim_ticket.OLD_OWNER_GROUP;
                    v_old_owner_usr := related_tahkim_ticket.OLD_OWNER;
                    v_owner_grp := related_tahkim_ticket.OWNER_GROUP;
                    v_owner_usr := related_tahkim_ticket.OWNER;
                END IF;

                if nvl(p_operation, 'X') = 'UPDATE_DOC_TYPE' then
                    v_new_subject := 'Evrak türü güncelleme sonucu yeniden atama gerçekleþti.';

                elsif v_old_owner_usr = v_owner_usr then
                    v_new_subject := 'Hukuk dosya no bilgisi güncellendi.';

                else
                    v_new_subject := 'Hukuk dosya no bilgisi güncelleme sonucu yeniden atama gerçekleþti.';
                end if;

                UPDATE ALZ_LAW_TICKET
                   SET LAWFILE_NO = DECODE(p_law_file_no, 'NULLL', NULL, p_law_file_no),
                       SIRKET_ADI = v_company_name,
                       COURT_FILE_NO = p_court_file_no,
                       SUBJECT = v_new_subject,
                       UPDATE_USER = p_user_name,
                       UPDATE_DATE = SYSDATE
                 WHERE TICKET_ID = related_tahkim_ticket.TICKET_ID;

                UPDATE ALZ_LAW_TICKET_PROCESS
                   SET DEADLINE_DATE = DECODE(NVL(v_bre_term, 0), 0, DEADLINE_DATE, (ARRIVAL_DATE + v_bre_term)),
                       DEPARTMENT = NVL(v_bre_department, DEPARTMENT),
                       OLD_OWNER = NVL(v_old_owner_usr, OLD_OWNER),
                       OLD_OWNER_GROUP = NVL(v_old_owner_grp, OLD_OWNER_GROUP),
                       OWNER = NVL(v_owner_usr, OWNER),
                       OWNER_GROUP = NVL(v_owner_grp, OWNER_GROUP),
                       UPDATE_USER = p_user_name,
                       UPDATE_DATE = SYSDATE,
                       REDIRECTOR = p_user_name,
                       RECEIVE_DATE = DECODE( v_owner_usr , related_tahkim_ticket.OWNER , RECEIVE_DATE , SYSDATE  )
                 WHERE TICKET_ID = related_tahkim_ticket.TICKET_ID;

                ALZ_LAW_TICKET_PROCESS_UTILS.INSERT_TICKET_HISTORY(related_tahkim_ticket.TICKET_ID, v_new_subject, p_user_name);

                v_employee_id := GET_EMPLOYEE_ID_BY_AZNET_NAME(v_old_owner_usr);
                v_employee_id_assigned := GET_EMPLOYEE_ID_BY_AZNET_NAME(v_owner_usr);
--                IF v_call_invoke_rest = 1 THEN
                    INVOKE_REST(null, 'http://esb.allianz.com.tr:12000/LitigationRest/GenericRestService/resources/myresource/REJECT/'||v_employee_id||'/'||p_ticket_id||'/'||v_employee_id_assigned||'',p_process_results);
--                END IF;

            END LOOP related_ticket_list;

        ELSIF v_source = 'MANUEL' THEN

            UPDATE ALZ_LAW_TICKET
               SET LAWFILE_NO = DECODE(p_law_file_no, 'NULLL', NULL, p_law_file_no), COURT_FILE_NO = p_court_file_no, LAW_COURT_ENF = p_court_enf_type, UYAP_BIRIM_ID = p_court_no
             WHERE TICKET_ID = p_ticket_id;
        ELSE
            alz_web_process_utils.process_result(
                1,
                0,
                9,
                -1,
                'REASSIGN_TICKET',
                v_source || ' iþleri için bu iþlem yapýlamaz.',
                v_source || ' iþleri için bu iþlem yapýlamaz.',
                null,
                null,
                'ALZ_LAW_TICKET_PROCESS_UTILS.REASSIGN_TICKET',
                p_process_results);

            RETURN;
        END IF;

    END;


    PROCEDURE GET_BRE_PARAMETERS (
        p_source_type varchar2,
        p_reAssign number default 0,
        p_bre_source varchar2,
        p_bre_is_matching varchar2,
        p_bre_code number,
        p_bre_lehte_aleyhte varchar2,
        p_bre_department varchar2,
        p_bre_in_lawyer_user  varchar2,
        p_bre_in_lawyer_group  varchar2,
        p_bre_in_lawyer_status varchar2,
        p_bre_out_lawyer_user  varchar2,
        p_bre_out_lawyer_group  varchar2,
        p_safahat_aciklama varchar2,
        p_bre_is_notification out varchar2,
        p_bre_send_mail out varchar2,
        p_bre_term out number,
        p_bre_is_ticket out varchar2,
        p_bre_group out varchar2,
        p_bre_user out varchar2,
        p_bre_message out varchar2 ) IS

    BEGIN

        IF p_reAssign = 0 THEN

            TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', 'yBEFORE-BRE_FIND_ASSINGMENT', P_bre_source || ' - ' || p_bre_is_matching || ' - ' || p_bre_code || ' - ' || p_bre_lehte_aleyhte || ' - ' || p_bre_department || ' - ' ||
                                     p_bre_in_lawyer_user || ' - ' || p_bre_in_lawyer_group || ' - ' || p_bre_in_lawyer_status || ' - ' || p_bre_out_lawyer_user || ' - ' || p_bre_out_lawyer_group || ' - ' || REPLACE(p_safahat_aciklama, 'Ý', 'I') || ' - ' ||
                                     p_bre_is_notification || ' - ' || p_bre_send_mail || ' - ' || p_bre_term || ' - ' || p_bre_is_ticket || ' - ' || p_bre_group || ' - ' ||
                                     p_bre_user || ' - ' || p_bre_message);

            LITIGATION_BRE_CLIENT.find_assign_user_with_group(p_bre_source, p_bre_is_matching, p_bre_code, p_bre_lehte_aleyhte, p_bre_department,
                                     p_bre_in_lawyer_user, p_bre_in_lawyer_group, p_bre_in_lawyer_status, p_bre_out_lawyer_user, p_bre_out_lawyer_group, REPLACE(p_safahat_aciklama, 'Ý', 'I'),
                                     p_bre_is_notification, p_bre_send_mail, p_bre_term, p_bre_is_ticket, p_bre_group,
                                     p_bre_user, p_bre_message);

            TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', 'zAFTER-BRE_FIND_ASSINGMENT', p_bre_source || ' - ' || p_bre_is_matching || ' - ' || p_bre_code || ' - ' || p_bre_lehte_aleyhte || ' - ' || p_bre_department || ' - ' ||
                                     p_bre_in_lawyer_user || ' - ' || p_bre_in_lawyer_group || ' - ' || p_bre_in_lawyer_status || ' - ' || p_bre_out_lawyer_user || ' - ' || p_bre_out_lawyer_group || ' - ' || REPLACE(p_safahat_aciklama, 'Ý', 'I') || ' - ' ||
                                     p_bre_is_notification || ' - ' || p_bre_send_mail || ' - ' || p_bre_term || ' - ' || p_bre_is_ticket || ' - ' || p_bre_group || ' - ' ||
                                     p_bre_user || ' - ' || p_bre_message);

        ELSE

            TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', 'yBEFORE-BRE_FIND_ASSINGMENT', P_bre_source || ' - ' || p_bre_is_matching || ' - ' || p_bre_code || ' - ' || p_bre_lehte_aleyhte || ' - ' || p_bre_department || ' - ' ||
                                     p_bre_in_lawyer_user || ' - ' || p_bre_in_lawyer_group || ' - ' || p_bre_in_lawyer_status || ' - ' || p_bre_out_lawyer_user || ' - ' || p_bre_out_lawyer_group || ' - ' || REPLACE(p_safahat_aciklama, 'Ý', 'I') || ' - ' ||
                                     p_bre_is_notification || ' - ' || p_bre_send_mail || ' - ' || p_bre_term || ' - ' || p_bre_is_ticket || ' - ' || p_bre_group || ' - ' ||
                                     p_bre_user || ' - ' || p_bre_message);

            LITIGATION_BRE_CLIENT.find_assignment_info  (p_bre_source, p_bre_is_matching, p_bre_code, p_bre_lehte_aleyhte, p_bre_department,
                                     p_bre_in_lawyer_user, p_bre_in_lawyer_group, p_bre_in_lawyer_status,  p_bre_out_lawyer_user, p_bre_out_lawyer_group, REPLACE(p_safahat_aciklama, 'Ý', 'I'),
                                     p_bre_is_notification, p_bre_send_mail, p_bre_term, p_bre_is_ticket, p_bre_group,
                                     p_bre_message);

            TMP_LOG('ALZ_LAW_TICKET_PROCESS_UTILS', 'zAFTER-BRE_FIND_ASSINGMENT', p_bre_source || ' - ' || p_bre_is_matching || ' - ' || p_bre_code || ' - ' || p_bre_lehte_aleyhte || ' - ' || p_bre_department || ' - ' ||
                                     p_bre_in_lawyer_user || ' - ' || p_bre_in_lawyer_group || ' - ' || p_bre_in_lawyer_status || ' - ' || p_bre_out_lawyer_user || ' - ' || p_bre_out_lawyer_group || ' - ' || REPLACE(p_safahat_aciklama, 'Ý', 'I') || ' - ' ||
                                     p_bre_is_notification || ' - ' || p_bre_send_mail || ' - ' || p_bre_term || ' - ' || p_bre_is_ticket || ' - ' || p_bre_group || ' - ' ||
                                     p_bre_user || ' - ' || p_bre_message);

        END IF;

    END;

    function get_status_control(p_ticket_id number) return varchar2
    is
        v_status varchar2(20);
        v_date date;
        v_type_id number;
        v_return varchar2(1000);
    begin
        select ticket_type_id,
               status,
               close_date
          into v_type_id,
               v_status,
               v_date
          from alz_law_ticket_process
         where ticket_id = p_ticket_id;

        if v_status = 'CLOSED' or v_date is not null then
            v_return := p_ticket_id||' nolu iþ kapalý olduðu için iþlem yapamazsýnýz.';

        elsif v_status = 'OPEN' then
            v_return := null;

        elsif v_status = 'CANCELLED' then
            v_return := p_ticket_id||' nolu iþ iptal edildiði için iþlem yapamazsýnýz..';

        elsif v_status = 'UNIFIED' then
            v_return := p_ticket_id||' nolu iþ birleþtirildiði için iþlem yapamazsýnýz.';

        elsif v_status = 'PAIRED' then
            v_return := p_ticket_id||' nolu iþ eþeleþtirildiði için iþlem yapamazsýnýz.';

        else
            v_return := p_ticket_id||' nolu iþ durumu hatalý olduðu için iþlem yapamazsýnýz.';
        end if;

        return v_return;

    exception when others then
        return p_ticket_id||' nolu iþ bulunamamýþtýr.';
    end get_status_control;

    function is_responsible(p_ticket_id number, p_user_name varchar2, p_source_location varchar2) return number
    is
        v_count number;
    begin

        select count(1)
          into v_count
          from alz_law_ticket t,
               alz_law_ticket_process tp,
               alz_law_group_def d
         where t.ticket_id = tp.ticket_id
           and t.ticket_id = p_ticket_id
           and tp.owner_group = d.code
           and (tp.owner = p_user_name
             or tp.owner_group in
               (
                select code
                  from alz_law_group_def
                 start with parent in
                 (
                     select group_code
                      from alz_law_group_user_rel
                     where validity_start_date <= sysdate
                       and (validity_end_date is null or validity_end_date >=sysdate)
                       and username = p_user_name
                 )
               connect by prior code = parent
             )
             or (exists
                 (select 1
                    from alz_law_group_user_rel a,
                         alz_law_group_def b
                   where a.username = p_user_name
                     and a.group_code = b.code
                     and b.is_ext_manager = 1)
                  and tp.old_owner_group in
                     (
                      select code
                        from alz_law_group_def
                       start with code in
                        (select group_code
                           from alz_law_group_user_rel
                          where validity_start_date <= sysdate
                            and (validity_end_date is null or validity_end_date >= sysdate)
                            and username = p_user_name)
                        connect by prior code = parent
                     ) and d.is_external = 1
             )
             or (nvl(p_source_location, 'X') = 'TICKET' and tp.close_date is not null)
           );

        if v_count > 0 then
            return 1;
        else
            return 0;
        end if;

    end is_responsible;

    function rucu_close_control(p_ticket_id number) return number
    is
        v_count number;
    begin

        select count(1)
          into v_count
          from alz_law_ticket t,
               clm_subfiles cs,
               koc_law_bases_detail lbd
         where t.ticket_id = p_ticket_id
           and t.rucu_file_no = cs.ext_reference
           and cs.claim_id = lbd.claim_id
           and cs.sf_no = lbd.sf_no;

        if v_count > 0 then
            return 1;
        else
            return 0;
        end if;

    end rucu_close_control;

END;
/

