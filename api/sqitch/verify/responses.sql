BEGIN;

  SELECT 
      id,               
      question_id,
      survey_id,       
      voto_id,         
      response_type,   
      content_type,    
      poll_id,         
      delivery_log_id, 
      choice_id,       
      subscriber_id,   
      subscriber_phone,
      question_title,  
      choice_name,     
      date_received     
    FROM farmradio_api.responses
  WHERE FALSE;

ROLLBACK;
