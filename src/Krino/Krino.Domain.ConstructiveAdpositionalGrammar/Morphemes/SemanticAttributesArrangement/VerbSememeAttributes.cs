using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.SemanticAttributesArrangement
{
    /// <summary>
    /// Semantic attributes of verbs.
    /// </summary>
    public class VerbSememeAttributes : EnumGroupBase
    {
        public VerbSememeAttributes(EnumGroupBase parent) : base(parent)
        {
            Person = new PersonAttributes(this);
            Number = new NumberAttributes(this);
            Tense = new TenseAttributes(this);
            Aspect = new AspectAttributes(this);
            Mood = new MoodAttributes(this);
            Voice = new VoiceAttributes(this);
        }

        public PersonAttributes Person { get; }

        public NumberAttributes Number { get; }

        public TenseAttributes Tense { get; }

        public AspectAttributes Aspect { get; }

        public MoodAttributes Mood { get; }

        public VoiceAttributes Voice { get; }
    }
}
