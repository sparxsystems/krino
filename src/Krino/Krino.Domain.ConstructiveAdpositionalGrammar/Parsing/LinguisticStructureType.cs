namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public enum LinguisticStructureType
    {
        Undefined,

        NonLexeme,
        Lexeme,
        
        Subject,

        Predicate,
        Verb,
        DirectObject,
        IndirectObject,
        SubjectComplement,
        ObjectComplement,
        AdjectiveComplement,
        AdverbialComplement,

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
