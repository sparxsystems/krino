namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public enum LinguisticStructureType
    {
        Undefined,

        NonLexeme,
        Lexeme,

        // Elements
        NounElement,
        AdjectiveElement,
        AttributiveAdjective,
        PredicativeAdjective,
        AdverbElement,

        // Objects
        DirectObject,
        IndirectObject,
        ObjectOfPreposition,

        // Complements
        SubjectComplement,
        ObjectComplement,
        AdjectiveComplement,
        AdverbialComplement,

        // Phrases
        PrepositionalPhrase,
        InfinitivePhrase,

        Subject,
        Verb,

        Predicate,

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
