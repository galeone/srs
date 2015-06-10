% SRS configuration file
% Each line is a method(parameter, weight) predicate. Weight must be a float number between 0 and 1.
% method results are changed according to:
% newMethodResult = parameterA * weightA + paremeterB * weightB + ...

% predicate topic_value: compute the numerical relation between a topic and a value in a range
topic_value(searched, 1.0).
topic_value(tagged, 1.0).
topic_value(rated_positive, 1.0).
topic_value(rated_negative, 1.0).
topic_value(commented, 1.0).
