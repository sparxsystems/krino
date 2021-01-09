using Krino.Domain.EnglishGrammar.Morphemes.Semantic;
using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.EnglishGrammar.Morphemes.Semantic
{
    /// <summary>
    /// Semantic attributes of verbs.
    /// </summary>
    public class VerbSememes : EnumGroupBase
    {
        public VerbSememes(EnumGroupBase parent) : base(parent)
        {
            Person = new PersonSememes(this);
            Number = new NumberSememes(this);
            Tense = new VerbTenseSememes(this);
            Aspect = new VerbAspectSememes(this);
            Mood = new VerbMoodSememes(this);
            Voice = new VerbVoiceSememes(this);
        }

        public PersonSememes Person { get; }

        public NumberSememes Number { get; }

        public VerbTenseSememes Tense { get; }

        public VerbAspectSememes Aspect { get; }

        public VerbMoodSememes Mood { get; }

        public VerbVoiceSememes Voice { get; }
    }
}
