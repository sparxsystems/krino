namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public enum LinguisticStructureType
    {
        Undefined,

        NonLexeme,
        Lexeme,

        NounElement,
        AdjectiveElement,
        AttributiveAdjective,
        PredicativeAdjective,
        AdverbElement,

        PrepositionalPhrase,

        Subject,

        Predicate,
        Verb,
        DirectObject,
        IndirectObject,
        ObjectOfPreposition,

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
