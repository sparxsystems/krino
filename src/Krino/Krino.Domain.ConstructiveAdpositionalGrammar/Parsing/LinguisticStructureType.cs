namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public enum LinguisticStructureType
    {
        Undefined,

        Lexeme,
        
        Subject,

        Predicate,
        Verb,
        DirectObject,
        IndirectObject,
        SubjectComplement,
        ObjectiveComplement,
        Adverbial,

        DeclarativeClause,
        InterrogativeClause,
        ImperativeClause,
        ExclamativeClause,
        NounClause,
        AdjectiveClause,
        AdverbialClause,
        
        SimpleSentence,
        ComplexSentence,
        CompoundSentence,
        CompoundComplexSentence
    }
}
