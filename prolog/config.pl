% SRS configuration file
% Each line is a method(parameter, weight) predicate. Weight must be a float number between 0 and 1.
% method results are changed according to:
% newMethodResult = parameterA * weightA + paremeterB * weightB + ...

% predicate action_coefficient: compute the numerical relation between a topic and a value in a range
action_coefficient(searched, 1.0).
action_coefficient(tagged, 1.0).
action_coefficient(rated_positive, 1.0).
action_coefficient(rated_negative, 1.0).
action_coefficient(commented, 1.0).
