namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface IClause : ILinguisticStructure
    {
        ITerm Subject { get; }
        ITerm Predicate { get; }
    }
}
