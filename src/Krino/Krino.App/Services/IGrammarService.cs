using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;

namespace Krino.App.Services
{
    public interface IGrammarService
    {
        Task<Parser> CreateParser();
    }
}
