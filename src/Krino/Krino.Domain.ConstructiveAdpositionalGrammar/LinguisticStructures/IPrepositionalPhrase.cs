namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface IPrepositionalPhrase : IPhrase
    {
        IWord Preposition { get; set; }

        INounElement ObjectOfPreposition { get; set; }
    }
}
