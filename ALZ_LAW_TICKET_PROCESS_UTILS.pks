CREATE OR REPLACE PACKAGE CUSTOMER."ALZ_LAW_TICKET_PROCESS_UTILS"
IS
   TYPE refcur IS REF CURSOR;

   PROCEDURE INSERT_TICKET_HISTORY (p_ticket_id   IN VARCHAR2,
                                    p_subject     IN VARCHAR2,
                                    p_user_name   IN VARCHAR2);

   PROCEDURE GET_TICKET_HISTORY (p_ticket_id IN NUMBER, cur OUT refcur);

   PROCEDURE GET_GROUPS (cur OUT refcur);

   PROCEDURE GET_GROUP_USERS (p_group_code IN VARCHAR2, cur OUT refcur);

   PROCEDURE INSERT_TICKET_NOTE (
      p_ticket_id         IN     NUMBER,
      p_note              IN     CLOB,
      p_is_internal       IN     NUMBER,
      p_create_user       IN     VARCHAR2,
      p_process_results      OUT customer.process_result_table
   );

   PROCEDURE GET_TICKET_NOTES (p_ticket_id   IN     NUMBER,
                               p_user_name   IN     VARCHAR2,
                               cur              OUT refcur);

   PROCEDURE GET_SAFAHAT_LIST (p_uyap_id        IN     NUMBER,
                               p_company_name   IN     VARCHAR2,
                               cur                 OUT refcur);

   PROCEDURE INSERT_THIRD_PARTY_DOC (
      p_ticket_id         IN     NUMBER,
      p_filenet_id        IN     VARCHAR2,
      p_user_name         IN     VARCHAR2,
      p_group             IN     VARCHAR2,
      p_process_results      OUT customer.process_result_table
   );

   PROCEDURE GET_SUPERIOR_EMAIL_BY_USER (
      p_user_name         IN     VARCHAR2,
      cur                    OUT refcur,
      p_process_results      OUT customer.process_result_table
   );

   PROCEDURE GET_EMAIL_BY_USER (p_user_name IN VARCHAR2, cur OUT refcur);

   PROCEDURE INSERT_DOCUMENT_CONTENTS (
      p_uyap_id              IN     NUMBER,
      p_document_id          IN     NUMBER,
      p_document_serial_no   IN     NUMBER,
      p_add_document         IN     VARCHAR2,
      p_filenet_id           IN     VARCHAR2,
      p_mime_type            IN     VARCHAR2,
      p_company_name         IN     VARCHAR2,
      p_process_results         OUT customer.process_result_table
   );

   FUNCTION UYAP_FILE_STATUS (uyap_file_id NUMBER, company_name VARCHAR2)
      RETURN VARCHAR2;

   PROCEDURE GET_TICKET_LIST (
      p_ticket_id                      IN     NUMBER,
      p_law_file_no                    IN     VARCHAR2,
      p_uyap_id                        IN     NUMBER,
      p_lawyer_part_id                 IN     NUMBER,
      p_in_lawyer                      IN     VARCHAR2,
      p_contact_person                 IN     VARCHAR2,
      p_contact_person_id              IN     NUMBER,
      p_file_open_date_start           IN     VARCHAR2,
      p_file_open_date_end             IN     VARCHAR2,
      p_ticket_close_date_start        IN     VARCHAR2,
      p_ticket_close_date_end          IN     VARCHAR2,
      p_opus_file_type                 IN     VARCHAR2,
      p_uyap_file_type                 IN     VARCHAR2,
      p_court_enf_no                   IN     VARCHAR2,
      p_court_enf_type                 IN     VARCHAR2,
      p_court_file_info                IN     VARCHAR2,
      p_receive_date_start             IN     VARCHAR2,
      p_receive_date_end               IN     VARCHAR2,
      p_status                         IN     VARCHAR2,
      p_parent_id                      IN     NUMBER,
      p_ticket_type                    IN     NUMBER,
      p_source                         IN     VARCHAR2,
      p_doc_type_code                  IN     NUMBER,
      p_create_date_start              IN     VARCHAR2,
      p_create_date_end                IN     VARCHAR2,
      p_folder_no                      IN     VARCHAR2,
      p_barcode_no                     IN     VARCHAR2,
      p_with_sub_tickets               IN     NUMBER,
      p_lawyer_reference               IN     VARCHAR2,
      p_user_name                      IN     VARCHAR2,
      cur                                 OUT refcur,
      p_process_results                   OUT customer.process_result_table
   );

   PROCEDURE GET_USER_STATUS (p_user IN VARCHAR2, p_status OUT VARCHAR2);

   PROCEDURE UPDATE_USER_STATUS (
      p_user              IN     VARCHAR2,
      p_status            IN     VARCHAR2,
      p_process_results   IN OUT CUSTOMER.PROCESS_RESULT_TABLE
   );

   PROCEDURE UPDATE_TICKET_STATUS (
      p_ticket_id         IN     NUMBER,
      p_status            IN     VARCHAR2,
      p_user_name         IN     VARCHAR2,
      p_process_results   IN OUT CUSTOMER.PROCESS_RESULT_TABLE
   );

   PROCEDURE UPDATE_ALL_TICKET_STATUS (
      p_ticket_array      IN     CUSTOMER.NUMBER_ARRAY,
      p_status            IN     VARCHAR2,
      p_user_name         IN     VARCHAR2,
      p_process_results   IN OUT customer.process_result_table
   );

   PROCEDURE UNIFY_TICKET (
      p_ticket_array      IN     CUSTOMER.NUMBER_ARRAY,
      p_user_name         IN     VARCHAR2,
      p_main_ticket_id       OUT NUMBER,
      p_process_results   IN OUT customer.process_result_table
   );

   PROCEDURE PAIR_TICKET (
      p_ticket_array      IN     CUSTOMER.NUMBER_ARRAY,
      p_user_name         IN     VARCHAR2,
      p_process_results      OUT customer.process_result_table
   );

   PROCEDURE SEPARATE_TICKET (
      p_ticket_array      IN     CUSTOMER.NUMBER_ARRAY,
      p_user_name         IN     VARCHAR2,
      p_process_results      OUT customer.process_result_table
   );

   PROCEDURE SET_DEADLINE (
      p_ticket_id         IN     NUMBER,
      p_deadline          IN     DATE,
      p_user_name         IN     VARCHAR2,
      p_process_results   IN OUT CUSTOMER.PROCESS_RESULT_TABLE
   );

   PROCEDURE SET_DOC_RECEIVED (
      p_ticket_id         IN     NUMBER,
      p_user_name         IN     VARCHAR2,
      p_process_results   IN OUT CUSTOMER.PROCESS_RESULT_TABLE
   );

--   PROCEDURE UPDATE_TICKET_LAWFILE_NO (
--      p_ticket_id         IN     NUMBER,
--      p_law_file_no       IN     VARCHAR2,
--      p_court_file_no     IN     VARCHAR2,
--      p_law_court_no      IN     VARCHAR2,
--      p_court_enf_type    IN     VARCHAR2,
--      p_user_name         IN     VARCHAR2,
--      p_process_results   IN OUT CUSTOMER.PROCESS_RESULT_TABLE
--   );

   PROCEDURE GET_OPUS_FILE_TYPES (CUR OUT REFCUR);

   PROCEDURE GET_UYAP_FILE_TYPES (CUR OUT REFCUR);

   PROCEDURE GET_TICKET_TYPE_LIST (CUR OUT REFCUR);

   FUNCTION GET_TIME_PASS (p_create_date     IN DATE,
                           p_deadline_DATE   IN DATE,
                           p_close_date      IN DATE)
      RETURN NUMBER;

   FUNCTION GET_TIME_LEFT (P_RECEIVE_DATE    IN DATE,
                           p_deadline_DATE   IN DATE,
                           p_close_date      IN DATE)
      RETURN NUMBER;

   PROCEDURE UPSERT_TICKET_PRIORITY (
      p_ticket_id         IN     NUMBER,
      p_user_name         IN     VARCHAR,
      p_priority_level    IN     NUMBER,
      p_process_results      OUT customer.process_result_table
   );

   PROCEDURE CREATE_TICKET (
      p_source_type       IN     VARCHAR2,
      p_assigned_user     IN     VARCHAR2,
      p_param_id          IN     VARCHAR2,
      p_deadline          IN     DATE,
      p_subject           IN     VARCHAR2,
      p_create_user       IN     VARCHAR2,
      p_arrival_date      IN     DATE,
      p_department        IN     VARCHAR2,
      p_new_ticket_id        OUT NUMBER,
      p_process_results      OUT customer.process_result_table
   );

   PROCEDURE CREATE_TICKET_PARAMETERS (
      p_source_type            IN     VARCHAR2,
      p_assigned_user          IN     VARCHAR2,
      p_param_id               IN     VARCHAR2,
      p_deadline               IN     DATE,
      p_subject                IN     VARCHAR2,
      p_create_user            IN     VARCHAR2,
      p_arrival_date           IN     DATE,
      p_department             IN     VARCHAR2,
      p_new_ticket_id          IN OUT NUMBER,
      v_ticket_type               OUT NUMBER,
      v_bre_group                 OUT VARCHAR2,
      v_bre_user                  OUT VARCHAR2,
      v_bre_message               OUT VARCHAR2,
      v_subject                   OUT VARCHAR2,
      v_bre_department            OUT VARCHAR2,
      v_category                  OUT VARCHAR2,
      v_law_file_no            IN OUT VARCHAR2,
      v_uyap_file_id              OUT NUMBER,
      v_filenet_id                OUT VARCHAR2,
      v_source                    OUT VARCHAR2,
      v_ticket_status             OUT VARCHAR2,
      v_law_court_enf             OUT VARCHAR2,
      v_court_file_no             OUT VARCHAR2,
      v_uyap_birim_id             OUT NUMBER,
      v_deadline                  OUT DATE,
      v_bre_term                  OUT NUMBER,
      v_rucu_no                   OUT VARCHAR2,
      v_company_name              OUT VARCHAR2,
      v_send_mail                 OUT NUMBER,
      v_uyap_safahat_id           OUT NUMBER,
      v_uyap_evrak_unique_id      OUT NUMBER,
      v_header_id                 OUT NUMBER,
      v_bre_code                  OUT NUMBER,
      v_arrival_date              OUT DATE,
      p_process_results           OUT customer.process_result_table,
      p_reAssign               IN     NUMBER default 0,
      p_check_bre_param        IN OUT NUMBER,
      p_create_ticket_or_notif IN OUT NUMBER
   );

   PROCEDURE REDIRECT_TICKET (
      p_assigned_group         IN     VARCHAR2,
      p_assigned_user          IN     VARCHAR2,
      p_redirector             IN     VARCHAR2,
      p_deadline               IN     DATE,
      p_subject                IN     VARCHAR2,
      p_redirect_reason_code   IN     NUMBER,
      p_ticket_id              IN     NUMBER,
      p_process_results           OUT customer.process_result_table
   );

   FUNCTION GET_DEPARTMENT (p_company_name     IN VARCHAR2,
                           p_court_enf_type   IN VARCHAR2,
                           p_court_enf_no     IN VARCHAR2,
                           p_code             IN NUMBER)
      RETURN VARCHAR2;

   FUNCTION GET_UYAP_EVRAK_DEPARTMENT (p_dosya_id      IN NUMBER,
                                    p_evrak_id      IN NUMBER,
                                    p_gg_evrak_id   IN NUMBER,
                                    p_sirket_adi    IN VARCHAR2)
      RETURN VARCHAR2;

   FUNCTION GET_LAWFILE_NO_FROM_COURT_NO (p_court_file_no VARCHAR2)
      RETURN VARCHAR2;

   PROCEDURE INSERT_TICKET_ASSIGNMENT_LOG;

   PROCEDURE UPSERT_LAW_DOC_TYPE_DEF (
      p_doc_name          IN     VARCHAR2,
      p_doc_type          IN     VARCHAR2,
      p_doc_source        IN     VARCHAR2,
      p_doc_code          IN     NUMBER,
      p_process_results      OUT customer.process_result_table
   );

   PROCEDURE GET_GROUPS (p_group_id     IN     VARCHAR2,
                         p_parent       IN     VARCHAR2,
                         p_group_name   IN     VARCHAR2,
                         p_assignable   IN     NUMBER,
                         cur               OUT refcur);

   PROCEDURE UPDATE_GROUP_INFO (p_group_id              IN VARCHAR2,
                                p_parent                IN VARCHAR2,
                                p_group_name            IN VARCHAR2,
                                p_assignable            IN NUMBER,
                                p_validity_start_date   IN DATE,
                                p_validity_end_date     IN DATE);

   PROCEDURE INSERT_ALZ_GROUP_INFO (
      p_group_id              IN     VARCHAR2,
      p_parent                IN     VARCHAR2,
      p_group_name            IN     VARCHAR2,
      p_validity_start_date   IN     DATE,
      p_validity_end_date     IN     DATE,
      p_assignable            IN     NUMBER,
      p_process_results          OUT customer.process_result_table
   );

   PROCEDURE GET_USERS (p_group_code   IN     VARCHAR2,
                        p_user_name    IN     VARCHAR2,
                        cur               OUT refcur);

   PROCEDURE INSERT_USER (
      p_group_code            IN     VARCHAR2,
      p_user_name             IN     VARCHAR2,
      p_validity_start_date   IN     DATE,
      p_validity_end_date     IN     DATE,
      p_process_results          OUT customer.process_result_table
   );

   PROCEDURE UPDATE_USER (
      p_group_code            IN     VARCHAR2,
      p_user_name             IN     VARCHAR2,
      p_validity_start_date   IN     DATE,
      p_validity_end_date     IN     DATE,
      p_oldgroup_code         IN     VARCHAR2,
      p_process_results          OUT customer.process_result_table
   );

   PROCEDURE GET_COURT_INFO (p_law_file_no IN VARCHAR2, cur OUT refcur);

   PROCEDURE GET_LAW_FILE (
      p_law_file_no                  IN     VARCHAR2,
      p_lawyer_part_id               IN     NUMBER,
      p_ext_reference                IN     VARCHAR2,
      p_recourse_included            IN     NUMBER,
      p_file_status                  IN     NUMBER,
      p_file_open_date_start         IN     VARCHAR2,
      p_file_open_date_end           IN     VARCHAR2,
      p_file_close_date_start        IN     VARCHAR2,
      p_file_close_date_end          IN     VARCHAR2,
      p_defendant_identity_no        IN     VARCHAR2,
      p_defendant_first_name         IN     VARCHAR2,
      p_defendant_surname            IN     VARCHAR2,
      p_plaintiff_identity_no        IN     VARCHAR2,
      p_plaintiff_first_name         IN     VARCHAR2,
      p_plaintiff_surname            IN     VARCHAR2,
      p_other_party_identity_no      IN     VARCHAR2,
      p_other_party_first_name       IN     VARCHAR2,
      p_other_party_surname          IN     VARCHAR2,
      p_opponent_identity_no         IN     VARCHAR2,
      p_in_lawyer                    IN     VARCHAR2,
      p_policy_ref                   IN     VARCHAR2,
      p_plate_no                     IN     VARCHAR2,
      p_agency_id                    IN     NUMBER,
      p_court_date_start             IN     VARCHAR2,
      p_court_date_end               IN     VARCHAR2,
      p_claim_date_start             IN     VARCHAR2,
      p_claim_date_end               IN     VARCHAR2,
      p_compesation_pay_date_start   IN     VARCHAR2,
      p_compesation_pay_date_end     IN     VARCHAR2,
      p_court_enf_no                 IN     VARCHAR2,
      p_court_enf_type               IN     VARCHAR2,
      p_court_file_info              IN     VARCHAR2,
      p_all_lawyers                  IN     NUMBER,
      p_lawyer_reference             IN     VARCHAR2,
      p_user_name                    IN     VARCHAR2,
      cur                               OUT refcur,
      p_process_results                 OUT customer.process_result_table
   );

   PROCEDURE GET_LAWYER_LIST (
      cur                 OUT refcur,
      p_process_results   OUT customer.process_result_table
   );

   PROCEDURE GET_IN_LAWYER_LIST (
      cur                 OUT refcur,
      p_process_results   OUT customer.process_result_table
   );

   PROCEDURE GET_OPPONENT_LAWYER_LIST (
      cur                 OUT refcur,
      p_process_results   OUT customer.process_result_table
   );

   PROCEDURE GET_FILE_FROM_UYAP (
      p_court_file_info           IN     VARCHAR2,
      p_defendant_identity_no     IN     VARCHAR2,
      p_defendant_first_name      IN     VARCHAR2,
      p_defendant_surname         IN     VARCHAR2,
      p_plaintiff_identity_no     IN     VARCHAR2,
      p_plaintiff_first_name      IN     VARCHAR2,
      p_plaintiff_surname         IN     VARCHAR2,
      p_other_party_identity_no   IN     VARCHAR2,
      p_other_party_first_name    IN     VARCHAR2,
      p_other_party_surname       IN     VARCHAR2,
      p_user_name                 IN     VARCHAR2,
      p_court_enf_no              IN     VARCHAR2,
      p_court_enf_type            IN     VARCHAR2,
      cur                            OUT refcur,
      p_process_results              OUT customer.process_result_table
   );

   PROCEDURE GET_COURT_LIST (cur OUT refcur);

   PROCEDURE GET_POLICY_REF_AND_EXT_REF (p_law_file_no   IN     VARCHAR2,
                                         cur                OUT refcur);

   PROCEDURE GET_PARTY_ROLE (cur OUT refcur);

   PROCEDURE GET_COMPANY_LIST_FROM_LOOK_UP (p_code   IN     VARCHAR2,
                                            p_cur       OUT refcur);

   PROCEDURE GET_DOCUMENT_TYPE_LIST (cur OUT refcur);

   PROCEDURE GET_CLAIM_DOCUMENT_TYPE_LIST (cur OUT refcur);

   PROCEDURE GET_FILTER_DOCUMENT_TYPE_LIST (cur OUT refcur);

   PROCEDURE GET_FILENET_MIMETYPE (p_uyap_mime_type      IN     VARCHAR2,
                                   p_filenet_mime_type      OUT VARCHAR2);

   PROCEDURE GET_UYAP_MIMETYPE (p_filenet_mime_type   IN     VARCHAR2,
                                p_uyap_mime_type         OUT VARCHAR2);

   PROCEDURE GET_DAY_LEFT (p_cur OUT refcur);

   PROCEDURE GET_DAY_LEFT_FOR_MUHABERAT (p_cur OUT refcur);

   PROCEDURE GET_EMAIL_FOR_UYAP_DOC (p_group IN VARCHAR2, cur OUT refcur);

   PROCEDURE GET_NEW_DOC_TYPE_FROM_UYAP (cur OUT refcur);

   PROCEDURE PROCEED_NEW_DOC_TYPE_FROM_UYAP;

   FUNCTION OWNER_BY_TICKETID (p_ticketId NUMBER)
      RETURN VARCHAR2;

   FUNCTION CHECK_TAHKIM_FOR_LAST_DAY
      RETURN VARCHAR2;

   FUNCTION GET_EMPLOYEE_ID_BY_AZNET_NAME (p_user_name IN VARCHAR2)
      RETURN VARCHAR2;

   FUNCTION GET_DOC_DESCRIPTION (p_doc_code IN NUMBER, p_source IN VARCHAR2)
      RETURN VARCHAR2;

   PROCEDURE IS_FILENET_ID_TO_UPDATE (p_filenet_id       VARCHAR2,
                                      p_cur          OUT refcur);

   PROCEDURE GET_GROUP_LEVEL_USERS_INFO (
      p_group_code        IN     VARCHAR2,
      p_user_name         IN     VARCHAR2,
      cur                    OUT refcur,
      p_process_results      OUT customer.process_result_table
   );

   PROCEDURE GET_TICKET_RESPONSIBLE_LIST (
      p_user_name         IN     VARCHAR2,
      mainCur                OUT refcur,
      childCur               OUT refcur,
      p_process_results      OUT customer.process_result_table
   );

   PROCEDURE GET_REASSIGNED_FILENETID_LIST(
      p_ticket_id       IN       NUMBER,
      p_source          IN       VARCHAR2,
      p_user_name       IN       VARCHAR2,
      cur                  OUT   refcur
   );

   PROCEDURE REASSIGN_TICKET (
      p_ticket_id         IN     NUMBER,
      p_law_file_no       IN     VARCHAR2,
      p_court_enf_type    IN     VARCHAR2,
      p_court_no          IN     NUMBER,
      p_court_file_no     IN     VARCHAR2,
      p_operation         IN     VARCHAR2,
      p_user_name         IN     VARCHAR2,
      p_process_results      OUT customer.process_result_table
   );

   PROCEDURE REDIRECT_RELATED_TICKETS(p_assigned_group       IN      VARCHAR2,
                                      p_assigned_user        IN      VARCHAR2,
                                      p_redirector           IN      VARCHAR2,
                                      p_law_file_no          IN      VARCHAR2,
                                      p_process_results         OUT     CUSTOMER.PROCESS_RESULT_TABLE);

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
        p_bre_message out varchar2 );

   function get_status_control(p_ticket_id number) return varchar2;

   function is_responsible(p_ticket_id number, p_user_name varchar2, p_source_location varchar2) return number;

   function rucu_close_control(p_ticket_id number) return number;
END;
/

