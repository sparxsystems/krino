namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface IClause : ILinguisticStructure, IPhraseItem
    {
        IWord Conjunction { get; set; }

        ISubject Subject { get; set; }

        IPredicate Predicate { get; set; }
    }
}
