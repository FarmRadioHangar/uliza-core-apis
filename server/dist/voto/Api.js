"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var Voto;
(function (Voto) {
    class Api {
        constructor(baseUrl = 'https://go.votomobile.org/api/v1/') {
            this.url = baseUrl.replace(/\/$/g, '');
            this.key = process.env.VOTO_KEY; // @TODO
        }
        /**
         * Return a well-formed url for to the provided VOTO API endpoint uri and
         * request parameters.
         *
         * @param endpoint A VOTO endpoint uri
         * @param params   GET request parameters
         *
         * @return A url which is valid for interacting with the VOTO API
         */
        buildUrl(endpoint, params) {
            const { limit, pageAfter, pageBefore } = params;
            let url = `${this.url}/${endpoint.replace(/^\//g, '')}?api_key=${this.key}&limit=${limit}`;
            if ('undefined' !== typeof pageAfter) {
                url += `&page_after=${pageAfter}`;
            }
            else if ('undefined' !== typeof pageBefore) {
                url += `&page_before=${pageBefore}`;
            }
            return url;
        }
    }
    Voto.Api = Api;
})(Voto = exports.Voto || (exports.Voto = {}));
