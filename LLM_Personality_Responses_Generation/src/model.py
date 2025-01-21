import openai
from openai import OpenAI
from tenacity import retry, stop_after_attempt, wait_exponential, retry_if_exception_type
import anthropic
from groq import Groq

class LLM:
    def __init__(self, api_key, model, model_name, voter):
        """
        Initialize the RaLLM class with the OpenAI API key.

        Parameter:
        - api_key (str): Your OpenAI or Claude API key.
        """
        self.model_type = model
        if model == 'gpt':
            self.client = OpenAI(api_key=api_key)
        elif model == 'claude':
            self.client = anthropic.Anthropic(api_key=api_key)
        elif model == 'llama':
            # self.client = Together(api_key=api_key)
            self.client = Groq(api_key=api_key)

        self.model_name = model_name
        self.voter = voter

    # this decorator is used to retry if the rate limits are exceeded
    @retry(
        reraise=True,
        stop=stop_after_attempt(1000),
        wait=wait_exponential(multiplier=1, min=4, max=10),
        retry=(retry_if_exception_type(openai.APITimeoutError)
               | retry_if_exception_type(openai.APIError)
               | retry_if_exception_type(openai.APIConnectionError)
               | retry_if_exception_type(openai.RateLimitError)),
    )
    def get_response(self, complete_prompt):
        """
        This function uses the OpenAI API to code a given sentence based on the provided complete_prompt.

        Parameters:
        - complete_prompt (str): The generated natural language prompt to be sent to the LLM.

        Returns:
        - list of str: The responses from the API.
        """
        # See API document at https://beta.openai.com/docs/api-reference/completions/create
        # max tokens: 100 is enough for single question. 
        # temperature: 0 for greedy (argmax).
        if self.model_type == 'gpt':
            response = self.client.chat.completions.create(
                model=self.model_name, #'gpt-3.5-turbo'
                max_tokens=1000,
                messages=[{"role": "user", "content": complete_prompt}],
                temperature=0.0,
                n=self.voter
            )
            return [response.choices[i].message.content for i in range(len(response.choices))]
        elif self.model_type == 'claude':
            response = self.client.messages.create(
                model=self.model_name, #"claude-3-opus-20240229, claude-3-haiku-20240307"
                max_tokens=1024,
                messages=[
                    {"role": "user", "content": complete_prompt}
                ],
                temperature=0.0,
            )
            return [response.content[i].text for i in range(len(response.content))]
        elif self.model_type == 'llama':
            # response = self.client.chat.completions.create(
            #     model='meta-llama/Meta-Llama-3-70B',
            #     messages=[{"role": "user", "content": complete_prompt}],
            #     temperature = 0.0,
            #     n=self.voter
            # )

            response = self.client.chat.completions.create(
                messages=[
                    {
                        "role": "user",
                        "content": complete_prompt,
                    }
                ],
                model="llama3-70b-8192",
                temperature=0.0,
                n=self.voter
            )
            return [response.choices[i].message.content for i in range(len(response.choices))]