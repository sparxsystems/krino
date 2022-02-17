namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface IVerbElement : IPhrase
    {
        IPhrase Verb { get; }

        IPhrase DirectObject { get; }
        IPhrase IndirectObject { get; }

        IPhrase ObjectComplement { get; }
        IPhrase AdverbialComplement { get; }
        IPhrase SubjectComplement { get; }
        IPhrase AdjectiveComplement { get; }

        IPhrase Adverbial { get; }
    }
}
