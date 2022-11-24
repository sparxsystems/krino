grammar English;

NOUN:       'noun';
VERB:       'verb';
ADJECTIVE:  'adjective';
ADVERB:     'adverb';
NUMERAL:    'numeral';

PRONOUN:    'pronoun';
DETETMINER: 'determiner';
PREPOSITION:    'preposition';
POSTPOSITION:   'postposition';

CONJUNCTION:    'conjunction';
CONJUNCTION_COORDINATING: 'conjunction.coordinating';

INTERJECTION:   'interjection';

WS:    (' ' | '\t')+  -> skip;


adjectivePhrase:    (adverbPhrase? ADJECTIVE)+ (CONJUNCTION_COORDINATING ADJECTIVE)*;

adverbPhrase:   ADVERB+ (CONJUNCTION_COORDINATING ADVERB)*;




