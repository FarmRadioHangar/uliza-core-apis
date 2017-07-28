BEGIN;

  CREATE TABLE farmradio_api.voto_responses (
    id                SERIAL      PRIMARY KEY,
    question_id       INT         NOT NULL,
    survey_id         INT         NOT NULL,
    voto_id           INT         NOT NULL,
    response_type     INT         NOT NULL,
    content_type      INT         NOT NULL,
    poll_id           INT         NOT NULL,
    delivery_log_id   INT         NOT NULL,
    choice_id         INT             NULL,
    subscriber_id     INT         NOT NULL,
    subscriber_phone  VARCHAR     NOT NULL,
    question_title    TEXT        NOT NULL,
    choice_name       TEXT            NULL,
    date_received     TIMESTAMPTZ NOT NULL
  );

  GRANT SELECT ON farmradio_api.voto_responses TO www;

  GRANT ALL ON farmradio_api.voto_responses TO admin;
  GRANT USAGE, SELECT ON SEQUENCE farmradio_api.voto_responses_id_seq TO admin;

COMMIT;
