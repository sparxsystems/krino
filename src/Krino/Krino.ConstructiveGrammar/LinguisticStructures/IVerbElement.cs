namespace Krino.ConstructiveGrammar.LinguisticStructures
{
    /// <summary>
    /// Element containing the verb phrase and all other modifiers related to the verb.
    /// </summary>
    public interface IVerbElement : IPhrase
    {
        IPhrase Verb { get; }

        IPhrase DirectObject { get; }
        IPhrase IndirectObject { get; }

        IPhrase ObjectComplement { get; }
        IPhrase AdverbialComplement { get; }
        IPhrase SubjectComplement { get; }
        IPhrase AdjectiveComplement { get; }

        IPhrase AdverbialAdjunct { get; }
    }
}
