namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface IClause : ILinguisticStructure
    {
        ISubject Subject { get; set; }

        IPredicate Predicate { get; set; }
    }
}
